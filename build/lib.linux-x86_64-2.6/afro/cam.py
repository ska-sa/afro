#! /usr/bin/env python
""" 
Selection of commonly-used control and monitoring functions for raw spead stream hardware.

Author: Jason Manley
"""
"""
Revisions:
2012-01-11: JRM Initial release
\n"""

import corr, time, sys, numpy, os, logging, katcp, struct, construct, socket, afro

DEFAULT_CONFIG='/etc/afro/default'
spead_time_id=0x1600

register_status = construct.BitStruct('status0',
    construct.Padding(32 - 29 - 1),     # 30 - 31
    construct.BitField("sync_val", 2),  # 28 - 29
    construct.Padding(28 - 17 - 1),     # 18 - 27
    construct.Flag('xaui_lnkdn'),       # 17
    construct.Flag('xaui_over'),        # 16
    construct.Padding(16 - 6 - 1),      # 7 - 15
    construct.Flag('dram_err'),         # 6 
    construct.Flag('clk_err'),          # 5 
    construct.Flag('adc_disabled'),     # 4 
    construct.Flag('fifo_overflow'),         # 3 
    construct.Flag('adc_overrange'),    # 2 
    construct.Flag('fft_overrange'),    # 1 
    construct.Flag('quant_overrange'))  # 0 

register_control = construct.BitStruct('control',
    construct.Padding(32 - 13 - 1),             # 21 - 31
    construct.Flag('adc_protect_disable'),      # 13
    construct.Flag('flasher_en'),               # 12
    construct.Padding(12 - 9 - 1),              # 10 - 11
    construct.Flag('gbe_enable'),               # 9 
    construct.Flag('gbe_rst'),                  # 8 
    construct.Padding(8 - 3 - 1),               # 4 - 7
    construct.Flag('clr_status'),               # 3 
    construct.Flag('arm'),                      # 2 
    construct.Flag('soft_sync'),                # 1 
    construct.Flag('mrst'))                     # 0 

snap_fpga_gbe_tx = construct.BitStruct("snap_gbe_tx0",
    construct.Padding(128 - 64 - 32 - 6), 
    construct.Flag("eof"),
    construct.Flag("link_up"),
    construct.Flag("led_tx"),
    construct.Flag("tx_full"),
    construct.Flag("tx_over"),
    construct.Flag("valid"),
    construct.BitField("ip_addr", 32),
    construct.BitField("data", 64))


def statsmode(inlist):
    """Very rudimentarily calculates the mode of an input list. Only returns one value, the first mode. Can't deal with ties!"""
    value=inlist[0]
    count=inlist.count(value)
    for i in inlist:
        if inlist.count(i) > count:
            value = i 
            count = inlist.count(i)
    return value 

def ip2str(pkt_ip, verbose = False):
    """
    Returns a dot notation IPv4 address given a 32-bit number.
    """
    ip_4 = (pkt_ip & ((2**32) - (2**24))) >> 24
    ip_3 = (pkt_ip & ((2**24) - (2**16))) >> 16
    ip_2 = (pkt_ip & ((2**16) - (2**8)))  >> 8
    ip_1 = (pkt_ip & ((2**8)  - (2**0)))  >> 0
    ipstr = '%i.%i.%i.%i' % (ip_4, ip_3, ip_2, ip_1)
    if verbose:
        print 'IP(%i) decoded to:' % pkt_ip, ipstr
    return ipstr
    
def write_masked_register(device_list, bitstruct, names = None, **kwargs):
    """
    Modify arbitrary bitfields within a 32-bit register, given a list of devices that offer the write_int interface - should be KATCP FPGA devices.
    """
    # lazily let the read function check our arguments
    currentValues = read_masked_register(device_list, bitstruct, names, return_dict = False)
    wv = []
    pulse_keys = []
    for c in currentValues:
        for key in kwargs:
            if not c.__dict__.has_key(key): raise RuntimeError('Attempting to write key %s but it doesn\'t exist in bitfield.' % key)
            if kwargs[key] == 'pulse':
                if pulse_keys.count(key) == 0: pulse_keys.append(key)
            else:
                c.__dict__[key] = (not c.__dict__[key]) if (kwargs[key] == 'toggle') else kwargs[key]
        bitstring = bitstruct.build(c)
        unpacked = struct.unpack('>I', bitstring)
        wv.append(unpacked[0])
    for d, device in enumerate(device_list):
        device.write_int(c.register_name, wv[d])
    # now pulse any that were asked to be pulsed
    if len(pulse_keys) > 0:
        #print 'Pulsing keys from write_... :(', pulse_keys
        pulse_masked_register(device_list, bitstruct, pulse_keys)

def read_masked_register(device_list, bitstruct, names = None, return_dict = True):
    """
    Read a 32-bit register from each of the devices (anything that provides the read_uint interface) in the supplied list and apply the given construct.BitStruct to the data.
    A list of Containers or dictionaries is returned, indexing the same as the supplied list.
    """
    if bitstruct == None: return
    if bitstruct.sizeof() !=  4: raise RuntimeError('Function can only work with 32-bit bitfields.')
    registerNames = names
    if registerNames == None:
        registerNames = []
        for d in device_list: registerNames.append(bitstruct.name)
    if len(registerNames) !=  len(device_list): raise RuntimeError('Length of list of register names does not match length of list of devices given.')
    rv = []
    for d, device in enumerate(device_list):
        vuint = device.read_uint(registerNames[d])
        rtmp = bitstruct.parse(struct.pack('>I', vuint))
        rtmp.raw = vuint
        rtmp.register_name = registerNames[d]
        if return_dict: rtmp = rtmp.__dict__
        rv.append(rtmp)
    return rv

def pulse_masked_register(device_list, bitstruct, fields):
    """
    Pulse a boolean var somewhere in a masked register.
    The fields argument is a list of strings representing the fields to be pulsed. Does NOT check Flag vs BitField, so make sure!
    http://stackoverflow.com/questions/1098549/proper-way-to-use-kwargs-in-python
    """
    zeroKwargs = {}
    oneKwargs = {}
    for field in fields:
      zeroKwargs[field] = 0
      oneKwargs[field] = 1
    #print zeroKwargs, '|', oneKwargs
    write_masked_register(device_list, bitstruct, **zeroKwargs)
    write_masked_register(device_list, bitstruct, **oneKwargs)
    write_masked_register(device_list, bitstruct, **zeroKwargs)

class Streamer:
    """A SPEAD streamer!"""
    def __init__(self, connect = True, config_file = None, log_handler = None, log_level = logging.INFO):
        self.log_handler = log_handler if log_handler != None else corr.log_handlers.DebugLogHandler(100)
        self.syslogger = logging.getLogger('streamersys')
        self.syslogger.addHandler(self.log_handler)
        self.syslogger.setLevel(log_level)

        if config_file == None: 
            config_file = DEFAULT_CONFIG
            self.syslogger.warn('Defaulting to config file %s.' % DEFAULT_CONFIG)
        self.config = afro.conf.StreamerConf(config_file)

        self.logger = logging.getLogger(self.config['server'])
        self.logger.addHandler(self.log_handler)
        self.logger.setLevel(log_level)

        self.logger.info('Configuration file %s parsed ok.' % config_file)

        if connect == True:
            self.connect()

    def connect(self):
        self.fpga=corr.katcp_wrapper.FpgaClient(self.config['server'],self.config['katcp_port'],timeout=10,logger=self.logger)
        time.sleep(1)
        if not self.check_katcp_connection():
            raise RuntimeError("Connection to FPGA boards failed.")
        #self.get_rcs()

    def __del__(self):
        self.disconnect_all()

    def disconnect_all(self):
        """Stop the TCP KATCP link to the FPGA defined in the config file."""
        try:
            self.fpga.stop()
        except:
            pass

    def get_rcs(self):
        """Extracts and returns a dictionary of the version control information from the FPGA bitstream."""
        try:
            rcs=self.fpga.get_rcs()
            if rcs.has_key('user'):
                self.logger.info('raw SPEAD streamer bitstream version %i found.'%rcs['user'])
            if rcs.has_key('compile_timestamp'):
                self.logger.info('bitstream was compiled at %s.'%time.ctime(frcs['compile_timestamp']))
            if rcs.has_key('app_last_modified'):
                self.logger.info('bitstream was last modified on %s.'%time.ctime(frcs['app_last_modified']))
            if rcs.has_key('lib_rcs_type'):
                self.syslogger.info('bitstream was compiled from %s DSP libraries, rev %0X %s.'%(
                    rcs['lib_rcs_type'],
                    rcs['lib_rev'],
                    ('DIRTY' if rcs['lib_dirty'] else 'CLEAN')))
            if rcs.has_key('app_rcs_type'):
                self.logger.info('bitstream was compiled from %s, rev %0X %s.'%(
                    rcs['app_rcs_type'],
                    rcs['app_rev'],
                    ('DIRTY' if frcs['app_dirty'] else 'CLEAN')))

            if self.config['adc_type'] == 'katadc':
                for an in range(self.config['n_inputs']/2):
                    adc_details=corr.katadc.eeprom_details_get(fpga,an)
                    self.logger.info("KATADC %i, rev %3.1f found on ZDOK port %i."%(adc_details['serial_number'],adc_details['pcb_rev']/10.0,an))

            return rcs
        except:
            self.syslogger.warn('Error retrieving RCS info from FPGA')
            return {}

    def label_input(self,input_n,ant_str):
        """Labels inputs as specified. input_n is an integer specifying the physical connection."""
        if input_n>=self.config['n_inputs']:
            raise RuntimeError("Trying to configure input %i? That's crazytalk, you only have %i inputs in your system!"%(input_n,self.config['n_inputs']))
        if ant_str=='' or ant_str==None:
            self.syslogger.warning('No antenna label specified, using defaults')
            ant_str='input%i'%(input_n)
        mapping=self.config._get_ant_mapping_list()
        mapping[input_n]=ant_str
        self.config.write_var_list('antenna_mapping',mapping)
        self.logger.info('Relabelled my input %i to %s.'%(input_n,ant_str))
        self.spead_labelling_issue()

    def prog_fpga(self):
        """Programs the FPGA."""
        #tested ok corr-0.5.0 2010-07-19
        self.fpga.progdev(self.config['bitstream'])
        if not self.check_fpga_comms(): 
            raise RuntimeError("Failed to successfully program FPGAs.")
        else:
            self.syslogger.info("All FPGAs programmed ok.")
            time.sleep(1)
            self.get_rcs()

    def input_mode_get(self):
        return ['x','y','xy','tvg'][self.fpga.read_uint('in_sel')]

    def input_mode_set(self,input_sel):
        """Select the analogue input. input_sel is the antenna string, or else special words 'dual' or 'both' to select dual 4-bit output or otherwise 'tvg' for the digital test vector generator."""
        if (input_sel == 'both') or (input_sel == 'dual'):
            input_n=2
        elif (input_sel == 'tvg'):
            input_n=3
        else:
            input_n=self.map_ant_to_input(input_sel)
        self.logger.info("Selecting input %i."%input_n)
        self.fpga.write_int('in_sel',input_n)

    def check_fpga_comms(self):
        """Checks FPGA <-> BORPH communications by writing a random number into a special register, reading it back and comparing."""
        #Modified 2010-01-03 so that it works on 32 bit machines by only generating random numbers up to 2**30.
        rv = True
        #keep the random number below 2^32-1 and do not include zero (default register start value), but use a fair bit of the address space...
        rn=numpy.random.randint(1,2**30)
        try: 
            self.fpga.write_int('sys_scratchpad',rn)
            self.logger.info("FPGA comms ok")
        except: 
            rv=False
            self.logger.error("FPGA comms failed")
        return rv

    def deprog_fpga(self):
        """Deprograms the FPGA."""
        #tested ok corr-0.5.0 2010-07-19
        self.fpga.progdev('')
        self.logger.info("All FPGAs deprogrammed.")

    def ctrl_set(self, **kwargs):
        write_masked_register([self.fpga], register_control, **kwargs)

    def ctrl_get(self):
        return read_masked_register([self.fpga], register_control)[0]

    def kitt_enable(self):
        """Turn on the Knightrider effect for system idle."""
        self.ctrl_set(flasher_en=True)

    def kitt_disable(self):
        """Turn off the Knightrider effect for system idle."""
        self.ctrl_set_all(flasher_en=False)

    def tvg_select(self):
        """ Turn the TVG on. """
        self.fpga.write_int('in_sel',3)

    def status_get_all(self):
        """Reads and decodes the status register from all the inputs."""
        rv={}
        for ant_str in self.config._get_ant_mapping_list():
            rv[ant_str] = self.status_get(ant_str)
        return rv

    def status_get(self,ant_str):
        """Reads and decodes the status register for a given antenna. Adds some other bits 'n pieces relating to input status."""
        gbe_n, input_n = self.get_ant_str_location(ant_str)
        rv = read_masked_register([self.fpga], register_status, names = ['status%i' % input_n])[0]
        if rv['xaui_lnkdn'] or rv['xaui_over'] or rv['clk_err'] or rv['fft_overrange']:
            rv['lru_state']='fail'
        elif rv['adc_overrange'] or rv['adc_disabled']:
            rv['lru_state']='warning'
        else:
            rv['lru_state']='ok'
        return rv

    def initialise(self, spead_id=-1, n_retries = 40, reprogram = True, clock_check = True, set_eq = True, config_10gbe = True, config_output = True, send_spead = True, prog_timeout_s = 5):
        """Initialises the system and checks for errors. User must provide the desired SPEAD ID for the output stream."""
        if reprogram:
            self.deprog_fpga()
            time.sleep(prog_timeout_s)
            self.prog_fpga()

        if self.tx_status_get(): self.tx_stop()

        self.gbe_reset_hold()

        if not self.arm(): self.syslogger.error("Failed to successfully arm and trigger system.")
        if clock_check == True: 
            if not self.check_clks(): 
                raise RuntimeError("System clocks are bad. Please fix and try again.")

        self.spead_id_set(spead_id)

        if self.config['adc_type'] == 'katadc': 
            self.rf_gain_set_all()

        if set_eq: self.eq_set_all()
        else: self.syslogger.info('Skipped EQ config.')

        if config_output: 
            self.config_udp_output()
            self.config_roach_10gbe_ports()

        self.gbe_reset_release()
        self.clr_status()
        time.sleep(1)

        stat=self.check_all(details=True)
        for in_n,ant_str in enumerate(self.config._get_ant_mapping_list()):
            if (stat[ant_str]['adc_disabled']==True) or (stat[ant_str]['adc_overrange']==True):
                self.logger.warn("%s input levels are too high!"%ant_str)

        if send_spead:
            self.spead_issue_all()

        self.kitt_enable()
        self.syslogger.info("Initialisation completed.")

    def gbe_reset_hold(self):
        """ Places the 10gbe core in reset. ALSO DISABLES ANY DATA OUTPUT TO THE CORE."""
        self.ctrl_set(gbe_enable = False, gbe_rst = True)
        self.logger.info("Holding 10GbE core(s) in reset.")

    def gbe_reset_release(self):
        """ Enables the 10gbe core. DOES NOT START DATA TRANSMISSION! """
        self.ctrl_set(gbe_enable = False, gbe_rst = False)
        self.logger.info("10GbE core(s) released from reset.")

    def tx_start(self):
        """Start outputting SPEAD products. Only works for systems with 10GbE output atm.""" 
        self.ctrl_set(gbe_enable = True)
        self.syslogger.info("Streamer output started.")
        if not self.check_10gbe_tx(): raise RuntimeError("10GbE cores are not transmitting properly.")

    def tx_stop(self,spead_stop=True):
        """Stops outputting SPEAD data over 10GbE links."""
        self.ctrl_set(gbe_enable = False)
        self.syslogger.info("Streamer output paused.")
        if spead_stop:
            import spead
            tx = spead.Transmitter(spead.TransportUDPtx(self.config['rx_meta_ip_str'], self.config['rx_udp_port']))
            tx.end()
            self.logger.info("Sent SPEAD end-of-stream notification.")
        else:
            self.logger.info("Did not send SPEAD end-of-stream notification.")

    def tx_status_get(self):
        """Returns boolean true/false if the hardware is currently outputting data."""
        rv=True
        stat=self.ctrl_get()
        if stat['gbe_enable'] != True or stat['gbe_rst']!=False: rv=False
        self.logger.info('Output is currently %s'%('enabled' if rv else 'disabled'))
        return rv

    def check_clks(self, quick_test=False):
        """ Checks FPGA's clk_frequency registers to confirm correct PPS operation. Requires that the system be sync'd. If quick_test is true, does not estimate the boards' clock frequencies."""
        rv = True
        expect_rate = round(self.config['fpga_clk'] / 1000000) # expected clock rate in MHz.

        # estimate actual clk freq 
        if quick_test == False:
            clk_freq = self.fpga.est_brd_clk()
            clk_mhz=round(clk_freq) #round to nearest MHz
            if clk_freq <= 100: 
                self.logger.error("No clock detected!")
                rv = False
            if (clk_mhz > (expect_rate+1)) or (clk_mhz < (expect_rate -1)) or (clk_mhz==0):
                self.logger.error("Estimated clock freq is %i MHz, where expected rate is %i MHz."%(clk_mhz, expect_rate))
                rv = False
            if rv != True: 
                self.logger.error("Bad FPGA clock. We can't continue.")
                return rv
        else: 
            self.syslogger.info("Fengine clocks are approximately correct at %i MHz."%expect_rate)

        #check long-term integrity
        #wait for within 100ms of a second, then delay a bit and query PPS count.
        ready=((int(time.time()*10)%10)==5)
        while not ready: 
            ready=((int(time.time()*10)%10)==5)
            #print time.time()
            time.sleep(0.05)
        uptime= self.fpga_uptime()[1]
        exp_uptime = numpy.floor(time.time() - self.config['sync_time'])

        if uptime == 0: 
            rv=False
            self.logger.error("No PPS detected! PPS count is zero.")
        elif uptime != exp_uptime: 
            rv=False
            self.logger.error("Expected uptime is %i seconds, but we've counted %i PPS pulses."%(exp_uptime,uptime))
        else:
            self.logger.info("Uptime is %i seconds, as expected."%(uptime))

        #check the PPS against sampling clock.
        clk_freq = self.fpga.read_uint('clk_frequency')
        modalfreq=numpy.round((expect_rate*1000000.)/clk_freq,3)
        if (modalfreq != 1):
            self.logger.error("PPS period is approx %3.2f Hz, not 1Hz (assuming a clock frequency of %i MHz)."%(modalfreq,expect_rate))
            rv=False
        else:
            self.logger.info("Assuming a clock of %iMHz, PPS period is %3.2fHz."%(expect_rate,modalfreq))
        if clk_freq == 0: 
            self.logger.error("No PPS or no clock... clk_freq register is zero!")
            rv=False
        if (clk_freq > (expect_rate*1000000+1)) or (clk_freq < (expect_rate*1000000 -1)) or (clk_freq==0):
            self.logger.error("Clocks between PPS pulses is %i, where we're expecting %i. This board has a bad sampling clock or PPS input."%(clk_freq, exp_rate*1000000))
            rv=False
        else:
            self.logger.info("Clocks between PPS pulses is %i as expected."%(clk_freq))
        return rv

    def fpga_uptime(self):
        """Returns a tuple of (armed_status and pps_count). Where the pps_count is since last arm of the pps signals received (and hence number of seconds since last arm)."""
        #tested ok corr-0.5.0 2010-07-19
        raw_value = self.fpga.read_uint('pps_count')
        pps_cnt = raw_value & 0x7FFFFFFF
        arm_stat = bool(raw_value & 0x80000000)
        return (arm_stat,pps_cnt)

    def mcnt_current_get(self):
        "Returns the current FPGA mcnt."
        msw = self.fpga.read_uint('mcount_msw')
        lsw = self.fpga.read_uint('mcount_lsw')
        return (msw << 32) + lsw 
    
    def pcnt_current_get(self):
        "Returns the current packet count."
        msw = self.fpga.read_uint('mcount_msw')
        lsw = self.fpga.read_uint('mcount_lsw')
        return int(((msw << 32) + lsw)*self.config['pcnt_scale_factor']/self.config['mcnt_scale_factor'])
    
    def arm(self, spead_update = True):
        """Arms the FPGA, records arm time and issues SPEAD update. Returns the UTC time at which the system was sync'd in seconds since the Unix epoch (MCNT=0)"""
        # wait for within 100ms of a half-second, then send out the arm signal.
        rv = True
        ready = ((int(time.time() * 10) % 10) == 5)
        while not ready: 
            ready = ((int(time.time() * 10) % 10) == 5)
        start_time = time.time()
        self.ctrl_set(arm = 'pulse')
        max_wait = self.config['sync_delay'] + 2
        #print 'Issued arm at %f.'%start_time
        done = False
        armed_stat = []
        while ((time.time() - start_time) < max_wait) and (not done):
            done = not self.fpga_uptime()[0]
            #print time.time(),done 
            time.sleep(0.1)
        done_time = time.time()
        if not done:
            self.logger.error("Did not trigger. Check clock and 1PPS.")
            rv = False
        else:
            self.logger.info('Triggered.')

        #print 'Detected trigger at %f.'%done_time
        self.config.write_var('sync_time', str(numpy.floor(done_time)))
        elapsed_time=numpy.floor(done_time)-numpy.ceil(start_time)
        if (elapsed_time) > self.config['sync_delay']:
            self.logger.error('We expected to trigger the boards in %i 1PPS pulses, but %i seconds have elapsed.'%(self.config['sync_delay'],elapsed_time))
            raise RuntimeError('We expected to trigger the boards in %i 1PPS pulses, but %i seconds have elapsed.'%(self.config['sync_delay'],elapsed_time))
        #print 'Recorded sync time as at %f.'%numpy.floor(done_time)
        if rv == False:
            self.logger.error("Failed to arm and trigger the system properly.")
            raise RuntimeError("Failed to arm and trigger the system properly.")
        if spead_update:
            self.spead_time_meta_issue()
        self.logger.info("Arm OK, sync time recorded as %i."%numpy.floor(done_time))
        return int(numpy.floor(done_time))

    def clr_status(self):
        """Clears the status registers and counters on FPGA."""
        pulse_masked_register([self.fpga], register_control, ['clr_status'])

    def check_katcp_connection(self):
        """Returns a boolean result of a KATCP ping to all all connected boards."""
        result = True
        try:
            if self.fpga.ping() == True:
                self.logger.info('KATCP connection ok.')
            else:
                self.logger.info('KATCP connection failed.')
                result=False
        except:
            self.logger.error('KATCP connection failure.')
            result = False
        return result

    def check_10gbe_tx(self):
        """Checks that the 10GbE cores are transmitting data. Outputs boolean good/bad."""
        rv=True
        stat=self.status_get_all()
        for in_n,ant_str in enumerate(self.config._get_ant_mapping_list()):
            gbe_n,input_n = self.get_ant_str_location(ant_str)
            if stat[(ant_str)]['xaui_lnkdn'] == True:
                self.logger.error("10GbE core %i for antenna %s link is down."%(gbe_n,ant_str))
                rv = False
            elif stat[(ant_str)]['xaui_over'] == True:
                self.logger.error('10GbE core %i for antenna %s is overflowing.'%(gbe_n,ant_str))
                rv = False
        for x in range(self.config['n_10gbe_ports']):
            firstpass_check = self.fpga.read_uint('gbe_tx_cnt%i'%x)
            time.sleep(0.01)
            secondpass_check = self.fpga.read_uint('gbe_tx_cnt%i'%x)
            if (secondpass_check == 0) or (secondpass_check == firstpass_check):
                self.logger.error('10GbE core %i is not sending any data.'%(x))
                rv = False
            else:
                self.logger.info('10GbE core %i is sending data.'%(x))

        if rv == True: self.logger.info("10GbE TX exchange check passed.")
        else: self.logger.error("Some 10GbE cores aren't sending data.")
        return rv

    def check_all(self,clock_check=False,details=False):
        """Checks system health. If 'details' is true, return a dictionary of results for each engine in the system. If details is false, returns boolean true if the system is operating nominally or boolean false if something's wrong."""
        rv={}
        rv.update(self.status_get_all())
        if not self.check_10gbe_tx(): rv['lru_state']='fail' 

        if clock_check:
            if not self.check_clks(): rv['lru_state']='fail' 

        if details:
            return rv
        else:
            return (True if rv['lru_state']=='ok' else False)



    def time_from_mcnt(self,mcnt):
        """Returns the unix time UTC equivalent to the input MCNT. Does NOT account for wrapping MCNT."""
        return self.config['sync_time']+float(mcnt)/self.config['mcnt_scale_factor']
        
    def mcnt_from_time(self,time_seconds):
        """Returns the mcnt of the correlator from a given UTC system time (seconds since Unix Epoch). Accounts for wrapping mcnt."""
        return int((time_seconds - self.config['sync_time'])*self.config['mcnt_scale_factor'])%(2**self.config['mcnt_bits'])

        #print 'Current Feng mcnt: %16X, uptime: %16is, target mcnt: %16X (%16i)'%(current_mcnt,uptime,target_pkt_mcnt,target_pkt_mcnt)
        
    def time_from_spead(self,spead_time):
        """Returns the unix time UTC equivalent to the input packet timestamp. Does not account for wrapping timestamp counters."""
        return self.config['sync_time']+float(spead_time)/float(self.config['spead_timestamp_scale_factor'])
        
    def spead_timestamp_from_time(self,time_seconds):
        """Returns the packet timestamp from a given UTC system time (seconds since Unix Epoch). Accounts for wrapping timestamp."""
        return int((time_seconds - self.config['sync_time'])*self.config['spead_timestamp_scale_factor'])%(2**(self.config['spead_flavour'][1]))

    def spead_id_set(self,spead_id=-1):
        """Sets item ID for the SPEAD stream (Numbers the spead_id software register.)"""
        if spead_id<0:
            spead_id=self.config['default_spead_id']
        self.fpga.write_int('spead_data_id', spead_id)
        self.fpga.write_int('spead_time_id', spead_time_id)
        self.logger.info('F engine board IDs set ok.')

    def spead_id_get(self):
        return self.fpga.read_uint('spead_data_id')

    def map_ant_to_input(self,ant_str):
        """Maps an antenna string to an input number."""
        try:
            input_n = self.config._get_ant_mapping_list().index(ant_str)
            return input_n
        except:
            self.syslogger.error('Unable to map antenna %s.'%ant_str)
            raise RuntimeError('Unable to map antenna %s.'%ant_str)
     
    def map_input_to_ant(self,input_n):
        """Maps an input number to an antenna string."""
        return self.config._get_ant_mapping_list()[input_n]

    def get_ant_str_location(self, ant_str):
        """ Returns the (10gbe_n,input_n) location of a given antenna."""
        return self.get_input_location(self.map_ant_to_input(ant_str))
        
    def get_input_location(self, input_n):
        " Returns the (10gbe_n,input_n) location for a given system-wide input number."
        if input_n > self.config['n_inputs'] or input_n < 0: 
            raise RuntimeError("There is no input %i in this design (total %i inputs)."%(input_n,self.config['n_inputs']))
        gbe_n  = 0 #HARDCODED TODO: generalise
        return (gbe_n, input_n)

    def config_roach_10gbe_ports(self):
        """Configures 10GbE ports on roach X (and F, if needed) engines for correlator data exchange using TGTAP."""
        for fc in range(self.config['n_10gbe_ports']):
            ip=self.config['10gbe_ip'] #TODO: Auto increment for additional future ports?
            mac = (2<<40) + (2<<32) + ip
            port=self.config['10gbe_port']
            self.fpga.tap_start('gbe%i'%fc,'gbe%i'%fc,mac,ip,port)
            # Set the Xengines' starting IP address.
            self.logger.info("Configured gbe%i core's IP address to %s"%(fc,ip2str(ip)))
                
#    def config_roach_10gbe_ports_static(self):
#        """STATICALLY configures 10GbE ports on roach X engines for correlator data exchange. Will not work with 10GbE output (we don't know the receiving computer's MAC)."""
#        arp_table=[(2**48)-1 for i in range(256)]
#
#        for f,fpga in enumerate(self.xfpgas):
#            for x in range(self.config['n_xaui_ports_per_xfpga']):
#                start_addr=self.config['10gbe_ip']
#                start_port=self.config['10gbe_port']
#                mac,ip,port=self.get_roach_gbe_conf(start_addr,(f*self.config['n_xaui_ports_per_xfpga']+x),start_port)
#                arp_table[ip%256]=mac
#
#        for f,fpga in enumerate(self.xfpgas):
#            for x in range(self.config['n_xaui_ports_per_xfpga']):
#                mac,ip,port=self.get_roach_gbe_conf(start_addr,(f*self.config['n_xaui_ports_per_xfpga']+x),start_port)
#                fpga.config_10gbe_core('gbe%i'%x,mac,ip,port,arp_table)
#                # THIS LINE SHOULD NOT BE REQUIRED WITH DAVE'S UPCOMING 10GBE CORE MODS
#                # Assign an IP address to each XAUI port's associated 10GbE core.
#                fpga.write_int('gbe_ip%i'%x, ip)

    def config_udp_output(self,dest_ip_str=None,dest_port=None):
        """Configures the destination IP and port for X engine output. dest_port and dest_ip are optional parameters to override the config file defaults. dest_ip is string in dotted-quad notation."""
        if dest_ip_str==None:
            dest_ip_str=self.config['rx_udp_ip_str']
        else:
            self.config['rx_udp_ip_str']=dest_ip_str
            self.config['rx_udp_ip']=struct.unpack('>L',socket.inet_aton(dest_ip_str))[0]
            self.config['rx_meta_ip_str']=dest_ip_str
            self.config['rx_meta_ip']=struct.unpack('>L',socket.inet_aton(dest_ip_str))[0]

        if dest_port==None:
            dest_port=self.config['rx_udp_port']
        else:
            self.config['rx_udp_port']=dest_port

        self.fpga.write_int('gbe_ip0',struct.unpack('>L',socket.inet_aton(dest_ip_str))[0])
        self.fpga.write_int('gbe_port',dest_port)
        self.logger.info("Correlator output configured to %s:%i." % (dest_ip_str, dest_port))
        #self.xwrite_int_all('gbe_out_pkt_len',self.config['rx_pkt_payload_len']) now a compile-time option

    def deconfig_roach_10gbe_ports(self):
        """Stops tgtap drivers."""
        self.fpga.tap_stop('gbe%i'%0) #TODO: Hard-coded to only support one core


    def get_gbe_tx_snapshot(self,snap_name = 'snap_gbe_tx0', offset = -1, man_trigger = False, man_valid = False):    
        raw = snapshots_get([self.fpga], dev_names = snap_name, wait_period = 3, circular_capture = False, man_trig = man_trigger, offset =     offset)     
        unp_rpt = construct.GreedyRepeater(snap_fpga_gbe_tx)    
        rv = []
        for index, d in enumerate(raw['data']):
            v= {}
            v['fpga_index'] = index
            v['data'] = unp_rpt.parse(d)
            rv.append(v)
        return rv

    def get_adc_snapshots(self,ant_strs,trig_level=-1,sync_to_pps=True):
        """Retrieves raw ADC samples from the specified antennas. Optionally capture the data at the same time. Optionally set a trigger level."""
        dev_names=[]
        for ant_str in ant_strs:    
            gbe_n, input_n = self.get_ant_str_location(ant_str)
            dev_names.append('adc_snap%i' % input_n) 

        init_mcnt = self.mcnt_current_get()
        mcnt_lsbs = init_mcnt & 0xffffffff

        if trig_level >= 0:
            self.fpga.write_int('trig_level', trig_level)
            raw = snapshots_get([self.fpga], dev_names, wait_period = -1, circular_capture = True, man_trig = (not sync_to_pps))
            ready = ((int(time.time() * 10) % 10) == 5)
            while not ready:
                time.sleep(0.05)
                ready = ((int(time.time() * 10) % 10) == 5)
        else:       
            raw = snapshots_get([self.fpga], dev_names, wait_period = 2, circular_capture = False, man_trig = (not sync_to_pps))
                
        rv = {}
        for ant_n, ant_str in enumerate(ant_strs):
            rv[ant_str] = {'data': numpy.fromstring(raw['data'][ant_n], dtype = numpy.int8), 'offset': raw['offsets'][ant_n], 'length': raw['lengths'][ant_n]}        
            ts = self.fpga.read_uint(dev_names[ant_n] + '_val')
            rv[ant_str]['timestamp'] = self.time_from_mcnt((init_mcnt & 0xffffffff00000000) + ts)
            if mcnt_lsbs > ts: 
                rv[ant_str]['timestamp'] += 0x100000000 # 32 bit number must've overflowed once.
        return rv
        
    def calibrate_adc_snapshot(self,ant_str,raw_data,n_chans=256):
        """Calibrates ADC count raw voltage input in timedomain. Returns samples in mV and a spectrum of n_chans in dBm."""
        adc_v=raw_data*self.config['adc_v_scale_factor']/(10**((self.rf_status_get(ant_str)[1])/20.))
        n_accs=len(adc_v)/n_chans/2
        freqs=numpy.arange(n_chans)*float(self.config['bandwidth'])/n_chans #channel center freqs in Hz. #linspace(0,float(bandwidth),n_chans) returns incorrect numbers
        window=numpy.hamming(n_chans*2)
        spectrum=numpy.zeros(n_chans)
        for acc in range(n_accs):
            spectrum += numpy.abs((numpy.fft.rfft(adc_v[n_chans*2*acc:n_chans*2*(acc+1)]*window)[0:n_chans])) 
        spectrum  = 20*numpy.log10(spectrum/n_accs/n_chans*4.91)
        return {'freqs':freqs,'spectrum_dbm':spectrum,'adc_v':adc_v}

    def rf_gain_set(self, ant_str, gain = None):
        """Enables the RF switch and configures the RF attenuators on KATADC boards. pol is ['x'|'y']. \n
        \t KATADC's valid range is -11.5 to 20dB. \n
        \t If no gain is specified, use the defaults from the config file."""
        #RF switch is in MSb.
        input_n = self.map_ant_to_input(ant_str)
        if self.config['adc_type'] != 'katadc' : 
            self.logger.error("RF gain cannot be configured on ADC type %s."%self.config['adc_type'])
            raise RuntimeError("Unsupported ADC type of %s. Only katadc is supported."%self.config['adc_type'])
        if gain == None:
            gain = self.config['rf_gain_%s' % (input_n)] 
        if gain > 20 or gain < -11.5:
             self.logger.error("Invalid gain setting of %i. Valid range for KATADC is -11.5 to +20")
             raise RuntimeError("Invalid gain setting of %i. Valid range for KATADC is -11.5 to +20")
        self.fpga.write_int('adc_ctrl%i' % input_n, (1<<31) + int((20 - gain) * 2))
        self.logger.info("Antenna %s on KATADC %i RF gain set to %2.1f." % (ant_str,input_n, round(gain * 2) / 2))

    def rf_status_get(self,ant_str):
        """Grabs the current value of the RF attenuators and RF switch state for KATADC boards. 
            Returns (enabled,gain in dB)"""
        #RF switch is in MSb.
        if self.config['adc_type'] != 'katadc' : 
            self.logger.warn("Can't get current RF status. Unsupported ADC type of %s. Only katadc is supported."%self.config['adc_type'])
            return (True,0.0)
        else:
            input_n = self.map_ant_to_input(ant_str)
            value = self.fpga.read_uint('adc_ctrl%i'%input_n)
            return (bool(value&(1<<31)),20.0-(value&0x3f)*0.5)

    def rf_status_get_all(self):
        """Grabs the current status of the RF chain on all KATADC boards."""
        #RF switch is in MSb.
        #tested ok corr-0.5.0 2010-07-19
        rv={}
        for in_n,ant_str in enumerate(self.config._get_ant_mapping_list()):
            rv[ant_str]=self.rf_status_get(ant_str)
        return rv

    def rf_gain_set_all(self,gain=None):
        """Sets the RF gain configuration of all inputs to "gain". If no level is given, use the defaults from the config file."""
        for ant_str in self.config._get_ant_mapping_list():
            self.rf_gain_set(ant_str, gain)

    def rf_disable(self,ant_str):
        """Disable the RF switch on KATADC boards."""
        #tested ok corr-0.5.0 2010-08-07
        #RF switch is in MSb.
        input_n = self.map_ant_to_input(ant_str)
        if self.config['adc_type'] != 'katadc' : 
            self.logger.warn("RF disable unsupported on ADC type of %s. Only katadc is supported at this time."%self.config['adc_type'])
        else:
            self.fpga.write_int('adc_ctrl%i'%input_n,self.fpga.read_uint('adc_ctrl%i'%input_n)&0x7fffffff)
            self.logger.info("Disabled RF frontend.")

    def rf_enable(self,ant_str):
        """Enable the RF switch on the KATADC board associated with requested antenna."""
        #RF switch is in MSb.
        input_n = self.map_ant_to_input(ant_str)
        if self.config['adc_type'] != 'katadc' : 
            self.logger.warn("RF enable unsupported on ADC type of %s. Only katadc is supported at this time."%self.config['adc_type'])
        else:
            self.fpga.write_int('adc_ctrl%i'%input_n,self.fpga.read_uint('adc_ctrl%i'%input_n)|0x80000000)
            self.logger.info("Enabled RF frontend.")

    def eq_set_all(self,eq_gain=-1):
        """Initialise all connected Fengines' EQs to given polynomial. If no polynomial or coefficients are given, use defaults from config file."""
        for in_n,ant_str in enumerate(self.config._get_ant_mapping_list()):
            self.eq_gain_set(ant_str=ant_str,eq_gain=eq_gain)
        self.logger.info('Set all EQ gains on all inputs.')

    def eq_default_get(self,ant_str):
        "Fetches the default equalisation configuration from the config file." 
        input_n  = self.map_ant_to_input(ant_str)
        return self.config['eq_gain_%i'%(input_n)]

    def eq_gain_set(self,ant_str,eq_gain=-1):
        """Set a given antenna's gain to the given co-efficient."""
        input_n = self.map_ant_to_input(ant_str)
        register_name='scale%i'%(input_n)
        if eq_gain <0:
            eq_gain = self.eq_default_get(ant_str)
        self.logger.info('''Initialising EQ for antenna %s, input %i, (register %s)'s to %i.'''%(ant_str,input_n,register_name,eq_gain))
        self.fpga.write_int(register_name,eq_gain)

    def eq_gain_get(self,ant_str):
        """Get a given antenna's current gain co-efficient."""
        input_n = self.map_ant_to_input(ant_str)
        register_name='scale%i'%(input_n)
        return self.fpga.read_uint(register_name)

    def adc_lru_mapping_get(self):
        """Map all the antennas to lru and physical inputs. Returns list of tuples of form (antenna_string, LRU, input_n on LRU)."""
        rv=[]
        for input_n,ant_str in enumerate(self.config._get_ant_mapping_list()):
            gbe_n,input_n = self.get_input_location(input_n)
            rv.append((ant_str,self.config['server'],input_n))
        return rv

    def adc_amplitudes_get(self, antstrs=[]):
        """Gets the ADC RMS amplitudes from the FPGA. If no antennas are specified, return all."""
        #Removed 'bits' cnt. Wasn't using it anywhere 'cos it wasn't exactly accurate. Rather use get_adc_snapshot and calc std-dev.
        #2011-04-20: JRM Changed "ants" to antpol so can specify any individual input.
        if antstrs == []:
            antstrs=self.config._get_ant_mapping_list()
        rv = {}
        for ant_str in antstrs:
            gbe_n,input_n = self.get_ant_str_location(ant_str)
            rv[ant_str] = {}
            rv[ant_str]['rms_raw'] = numpy.sqrt(self.fpga.read_uint('adc_sum_sq%i'%(input_n))/float(self.config['adc_levels_acc_len']))
            rv[ant_str]['rms_v'] = rv[ant_str]['rms_raw']*self.config['adc_v_scale_factor']
            rv[ant_str]['adc_rms_dbm'] = v_to_dbm(rv[ant_str]['rms_v'])
            rf_status=self.rf_status_get(ant_str) 
            rv[ant_str]['analogue_gain'] = rf_status[1]
            rv[ant_str]['input_rms_dbm'] = rv[ant_str]['adc_rms_dbm']-rv[ant_str]['analogue_gain']
            rv[ant_str]['low_level_warn'] = True if (rv[ant_str]['adc_rms_dbm']<self.config['adc_low_level_warning']) else False
            rv[ant_str]['high_level_warn'] = True if (rv[ant_str]['adc_rms_dbm']>self.config['adc_high_level_warning']) else False
        return rv

    def spead_labelling_issue(self):
        import spead
        tx=spead.Transmitter(spead.TransportUDPtx(self.config['rx_meta_ip_str'], self.config['rx_udp_port']))
        ig=spead.ItemGroup()

        ig.add_item(name="input_labelling",id=0x100E,
            description="The physical location of each antenna connection.",
            #shape=[self.config['n_inputs']],fmt=spead.STR_FMT,
            init_val=numpy.array([(ant_str,lru,input_n) for (ant_str,lru,input_n) in self.adc_lru_mapping_get()]))

        tx.send_heap(ig.get_heap())
        self.syslogger.info("Issued SPEAD metadata describing baseline labelling and input mapping to %s:%i."%(self.config['rx_meta_ip_str'],self.config['rx_udp_port']))


    def spead_static_meta_issue(self):
        """ Issues the SPEAD metadata packets containing the payload and options descriptors and unpack sequences."""
        import spead
        #tested ok corr-0.5.0 2010-08-07
        tx=spead.Transmitter(spead.TransportUDPtx(self.config['rx_meta_ip_str'],self.config['rx_udp_port']))
        ig=spead.ItemGroup()

        ig.add_item(name="adc_clk",id=0x1007,
            description="Clock rate of ADC (samples per second).",
            shape=[],fmt=spead.mkfmt(('u',64)),
            init_val=self.config['adc_clk'])

        ig.add_item(name="n_inputs",id=0x3100,
            description="The number of inputs in the stream.",
            shape=[],fmt=spead.mkfmt(('u',spead.ADDRSIZE)),
            init_val=(2 if (self.input_mode_get() == 'xy') else 1))

        ig.add_item(name="pkt_len",id=0x3103,
            description="Payload length of output packets.",
            shape=[],fmt=spead.mkfmt(('u',spead.ADDRSIZE)),
            init_val=self.config['rx_pkt_payload_len'])

        ig.add_item(name="center_freq",id=0x1011,
            description="The center frequency of the DBE in Hz, 64-bit IEEE floating-point number.",
            shape=[],fmt=spead.mkfmt(('f',64)),
            init_val=self.config['center_freq'])

        ig.add_item(name="bandwidth",id=0x1013,
            description="The analogue bandwidth of the digitally processed signal in Hz.",
            shape=[],fmt=spead.mkfmt(('f',64)),
            init_val=self.config['bandwidth'])
        
        ig.add_item(name="t_bits_per_sample",id=0x1048,
            description="Number of bits after requantisation.",
            shape=[],fmt=spead.mkfmt(('u',spead.ADDRSIZE)),
            init_val=(4 if (self.input_mode_get() == 'xy') else self.config['adc_n_bits']))

        ig.add_item(name="rx_udp_port",id=0x1022,
            description="Destination UDP port for X engine output.",
            shape=[],fmt=spead.mkfmt(('u',spead.ADDRSIZE)),
            init_val=self.config['rx_udp_port'])

        ig.add_item(name="rx_udp_ip_str",id=0x1024,
            description="Destination IP address for X engine output UDP packets in dotted-quad notation.",
            shape=[-1],fmt=spead.STR_FMT,
            init_val=self.config['rx_udp_ip_str'])

        ig.add_item(name="ddc_mix_freq",id=0x1043,
            description="Digital downconverter mixing freqency as a fraction of the ADC sampling frequency. eg: 0.25. Set to zero if no DDC is present.",
            shape=[],fmt=spead.mkfmt(('f',64)),
            init_val=self.config['ddc_mix_freq'])

        ig.add_item(name="adc_n_bits",id=0x1045,
            description="ADC resolution/quantisation (number of bits).",
            shape=[],fmt=spead.mkfmt(('u',spead.ADDRSIZE)),
            init_val=self.config['adc_n_bits'])

        if self.input_mode_get()=='x' or opts.in_sel=='y':
            ant=self.map_input_to_ant({'x':0,'y':1}[self.input_mode_get()])
            ig.add_item(name=("raw_data_%s"%(ant)),id=self.spead_id_get(),
                description="Raw data stream from the ADC(s). Each sample represents a signed integer value.",
                shape=[4096],fmt=spead.mkfmt(('u',8)))
        elif self.input_mode_get()=='xy':
            ig.add_item(name=("raw_data_%s_%s"%(r.config._get_ant_mapping_list()[0],r.config._get_ant_mapping_list()[1])),id=self.spead_id_get(),
                description="Raw data stream from the ADC(s). Each sample represents a signed integer value.",
                shape=[4096],fmt=spead.mkfmt(('i',4),('i',4)))
        else:
            raise RuntimeError("input selection not valid.")

        tx.send_heap(ig.get_heap())
        self.syslogger.info("Issued misc SPEAD metadata to %s:%i."%(self.config['rx_meta_ip_str'],self.config['rx_udp_port']))

    def spead_time_meta_issue(self):
        """Issues a SPEAD packet to notify the receiver that we've resync'd the system, acc len has changed etc."""
        #tested ok corr-0.5.0 2010-08-07
        import spead
        tx=spead.Transmitter(spead.TransportUDPtx(self.config['rx_meta_ip_str'],self.config['rx_udp_port']))
        ig=spead.ItemGroup()

        ig.add_item(name='sync_time',id=0x1027,
            description="Time at which the system was last synchronised (armed and triggered by a 1PPS) in seconds since the Unix Epoch.",
            shape=[],fmt=spead.mkfmt(('u',spead.ADDRSIZE)),
            init_val=self.config['sync_time'])

        ig.add_item(name="scale_factor_timestamp",id=0x1046,
            description="Timestamp scaling factor. Divide the SPEAD data packet timestamp by this number to get back to seconds since last sync.",
            shape=[],fmt=spead.mkfmt(('f',64)),
            init_val=self.config['spead_timestamp_scale_factor'])

        ig.add_item(name=('timestamp'), id=spead_time_id,
            description='Timestamp of the first sample in this packet. uint counting multiples of ADC samples since last sync (sync_time, id=0x1027). Divide this number by timestamp_scale (id=0x1046) to get back to seconds since last sync.',
            shape=[], fmt=spead.mkfmt(('u',spead.ADDRSIZE)),
            init_val=0)

        tx.send_heap(ig.get_heap())
        self.syslogger.info("Issued SPEAD timing metadata to %s:%i."%(self.config['rx_meta_ip_str'],self.config['rx_udp_port']))

    def spead_eq_meta_issue(self):
        """Issues a SPEAD heap for the RF gain and EQ settings."""
        import spead
        tx=spead.Transmitter(spead.TransportUDPtx(self.config['rx_meta_ip_str'],self.config['rx_udp_port']))
        ig=spead.ItemGroup()

        if self.config['adc_type'] == 'katadc':
            for input_n,ant_str in enumerate(self.config._get_ant_mapping_list()):
                ig.add_item(name="rf_gain_%i"%(input_n),id=0x1200+input_n,
                    description="The analogue RF gain applied at the ADC for input %i (ant %s) in dB."%(input_n,ant_str),
                    shape=[],fmt=spead.mkfmt(('f',64)),
                    init_val=self.config['rf_gain_%i'%(input_n)])

        if self.input_mode_get()=='xy':
            for in_n,ant_str in enumerate(self.config._get_ant_mapping_list()):
                    ig.add_item(name="eq_gain_%s"%(ant_str),id=0x1400+in_n,
                        description="The unitless digital scaling factor implemented prior to requantisation for input %s. 32 bit integers."%(ant_str),
                        shape=[],fmt=spead.mkfmt(('u',32)),
                        init_val=self.config['eq_gain_%i'%(input_n)])
        else:
            ig.add_item(name="eq_gain",id=0x1400,
                description="The unitless digital scaling factor implemented prior to requantisation for input %s. 32 bit integers."%(ant_str),
                shape=[],fmt=spead.mkfmt(('u',32)),
                init_val=1)

        tx.send_heap(ig.get_heap())
        self.syslogger.info("Issued SPEAD EQ metadata to %s:%i."%(self.config['rx_meta_ip_str'],self.config['rx_udp_port']))

    def spead_issue_all(self):
        """Issues all SPEAD metadata."""
        self.spead_static_meta_issue()
        self.spead_time_meta_issue()
        self.spead_eq_meta_issue()
        self.spead_labelling_issue()

    def is_wideband(self):
        return self.config['mode'] == self.MODE_WB

    def is_narrowband(self):
        return self.config['mode'] == self.MODE_NB
    
    def is_ddc(self):
        return self.config['mode'] == self.MODE_DDC

def dbm_to_dbuv(dbm):
    return dbm+107

def dbuv_to_dbm(dbuv):
    return dbm-107

def v_to_dbuv(v):
    return 20*numpy.log10(v*1e6)

def dbuv_to_v(dbuv):
    return (10**(dbuv/20.))/1e6

def dbm_to_v(dbm):
    return numpy.sqrt(10**(dbm/10.)/1000*50)

def v_to_dbm(v):
    return 10*numpy.log10(v*v/50.*1000)


def snapshots_get(fpgas,dev_names,man_trig=False,man_valid=False,wait_period=-1,offset=-1,circular_capture=False):
    """Fetches data from multiple snapshot devices. fpgas and dev_names are lists of katcp_wrapper.FpgaClient, and 'snapshot_device_name', respectively.
        This function triggers and retrieves data from the snap block devices. The actual captured length and starting offset is returned with the dictionary of data for each FPGA (useful if you've done a circular capture and can't calculate this yourself).\n
        \tdev_names: list of strings, names of the snap block corresponding to FPGA list. Can optionally be 1-D, in which case name is used for all FPGAs.\n
        \tman_trig: boolean, Trigger the snap block manually.\n
        \toffset: integer, wait this number of valids before beginning capture. Set to negative if your hardware doesn't support offset triggering or to leave the register alone. Note that you should explicitly set this to zero to start directly after a trigger because by default (negative), it will remember the last-set offset value.\n
        \tcircular_capture: boolean, Enable the circular capture function.\n
        \twait_period: integer, wait this number of seconds between triggering and trying to read-back the data. Make it negative to   wait forever.\n
        \tRETURNS: dictionary with keywords: \n
        \t\tlengths: list of integers matching number of valids captured off each fpga.\n
        \t\toffset: optional (depending on snap block version) list of number of valids elapsed since last trigger on each fpga.
        \t\t{brams}: list of data from each fpga for corresponding bram.\n
        """
    # 2011-06-24 JRM first write. 
    if isinstance(dev_names,str):
        dev_names=[dev_names for f in fpgas]

    if offset >=0:
        for fn,fpga in enumerate(fpgas):
            fpga.write_int(dev_names[fn]+'_trig_offset',offset)

    for fn,fpga in enumerate(fpgas):
        fpga.write_int(dev_names[fn]+'_ctrl',(0 + (man_trig<<1) + (man_valid<<2) + (circular_capture<<3)))
        fpga.write_int(dev_names[fn]+'_ctrl',(1 + (man_trig<<1) + (man_valid<<2) + (circular_capture<<3)))

    done=False
    start_time=time.time()
    while not done and ((time.time()-start_time)<wait_period or (wait_period < 0)):
        addr      = [fpga.read_uint(dev_names[fn]+'_status') for fn,fpga in enumerate(fpgas)]
        done_list = [not bool(i & 0x80000000) for i in addr]
        if (done_list == [True for i in fpgas]): done=True

    bram_dmp=dict()
    bram_dmp['data']=[]
    bram_dmp['lengths']=[i&0x7fffffff for i in addr]
    bram_dmp['offsets']=[0 for fn in fpgas]
    for fn,fpga in enumerate(fpgas):
        now_status=bool(fpga.read_uint(dev_names[fn]+'_status')&0x80000000)
        now_addr=fpga.read_uint(dev_names[fn]+'_status')&0x7fffffff
        if (bram_dmp['lengths'][fn] != now_addr) or (bram_dmp['lengths'][fn]==0) or (now_status==True):
            #if address is still changing, then the snap block didn't finish capturing. we return empty.  
            raise RuntimeError("A snap block logic error occurred on capture #%i. It reported capture complete but the address is      either still changing, or it returned 0 bytes captured after the allotted %2.2f seconds. Addr at stop time: %i. Now: Still running :   %s, addr: %i."%(fn,wait_period,bram_dmp['lengths'][fn],{True:'yes',False:'no'}[now_status],now_addr))
            bram_dmp['lengths'][fn]=0
            bram_dmp['offsets'][fn]=0

        if circular_capture:
            bram_dmp['offsets'][fn]=fpga.read_uint(dev_names[fn]+'_tr_en_cnt') - bram_dmp['lengths'][fn]
        else:
            bram_dmp['offsets'][fn]=0

        if bram_dmp['lengths'][fn] == 0:
            bram_dmp['data'].append([])
        else:
            bram_dmp['data'].append(fpga.read(dev_names[fn]+'_bram',bram_dmp['lengths'][fn]))

    bram_dmp['offsets']=numpy.add(bram_dmp['offsets'],offset)

    for fn,fpga in enumerate(fpgas):
        if (bram_dmp['offsets'][fn]<0):
            bram_dmp['offsets'][fn]=0

    return bram_dmp
