import iniparse, exceptions, socket, struct, numpy, os
"""
Library for parsing configuration files

Author: Jason Manley
"""
"""
Revs:
2012-06-01 Initial revision
"""
LISTDELIMIT = ','
PORTDELIMIT = ':'

VAR_RUN = '/var/run/afro'

class StreamerConf:    
    def __init__(self, config_file):
        self.config_file = config_file
        self.config_file_name = os.path.split(self.config_file)[1]
        self.cp = iniparse.INIConfig(open(self.config_file, 'rb'))
        self.config = dict()
        self.read_common()

    def __getitem__(self, item):
        if item == 'sync_time':
            fp = open(VAR_RUN + '/' + item + '.' + self.config_file_name, 'r')
            val = float(fp.readline())
            fp.close()
            return val
        elif item == 'antenna_mapping':
            fp = open(VAR_RUN + '/' + item + '.' + self.config_file_name, 'r')
            val = (fp.readline()).split(LISTDELIMIT)
            fp.close()
            return val 
        else:
            return self.config[item]

    def __setitem__(self,item,value):
        self.config[item]=value

    def keys():
        return sorted(self.config.keys())

    def file_exists(self):
        try:
            #f = open(self.config_file)
            f = open(self.config_file, 'r')
        except IOError:
            exists = False
            raise RuntimeError('Error opening config file at %s.'%self.config_file)
        else:
            exists = True
            f.close()

        # check for runtime files and create if necessary:
        if not os.path.exists(VAR_RUN):
            os.mkdir(VAR_RUN)
            #os.chmod(VAR_RUN,0o777)
        for item in ['sync_time','antenna_mapping']:
            if not os.path.exists(VAR_RUN + '/' + item + '.' + self.config_file_name):
                f = open(VAR_RUN + '/' + item + '.' + self.config_file_name, 'w')
                f.write(chr(0))
                f.close()
                #os.chmod(VAR_RUN+'/' + item,0o777)
        return exists

    def _get_ant_mapping_list(self):
        ant_list = self['antenna_mapping']
        if len(ant_list) < self.config['n_inputs']:
            #there's no current mapping or the mapping is bad... set default:
            ant_list=[]
            for a in range(self.config['n_inputs']):
                ant_list.append('in%i'%(a))
        return ant_list[0:self.config['n_inputs']]

    def map_ant_to_input(self,ant_str):
        """Maps an antenna string to an input number."""
        try:
            input_n = self._get_ant_mapping_list().index(ant_str)
            return input_n
        except:
            raise RuntimeError('Unable to map antenna')

    def map_input_to_ant(self,input_n):
        """Maps an input number to an antenna string."""
        return self._get_ant_mapping_list()[input_n]


    def read_common(self):
        if not self.file_exists():
            raise RuntimeError('Error opening config file or runtime variables.')

        #get the server stuff
        self.read_int('katcp','katcp_port')
        if len(self.cp.katcp.server.strip()) > 0:
          self.config['server'] = self.cp.katcp.server.strip()
        else:
          raise RuntimeError("Warning, no server found in config file.")
          self.config['servers_f'] = []
        self.config['bitstream'] = self.cp.katcp.bitstream

        self.read_int('fpga','adc_levels_acc_len')
        self.read_int('fpga','10gbe_port')
        self.read_int('fpga','pcnt_bits')
        self.read_int('fpga','mcnt_bits')
        self.read_int('fpga','adc_clk')
        self.read_int('fpga','n_inputs')
        self.read_int('fpga','sync_delay')
        self.read_int('fpga','n_10gbe_ports')
        self.read_int('fpga','ddc_mix_freq')
        self.read_int('fpga','ddc_decimation')

        self.config['10gbe_ip']=struct.unpack('>I',socket.inet_aton(self.get_line('fpga','10gbe_ip')))[0]

        # determine the bandwidth the system is processing
        self.config['rf_bandwidth'] = self.config['adc_clk'] / 2.

        # is a DDC being used in the F engine?
        if self.config['ddc_mix_freq'] > 0:
            self.config['bandwidth'] = float(self.config['adc_clk']) / self.config['ddc_decimation']
            self.config['center_freq'] = float(self.config['adc_clk']) * self.config['ddc_mix_freq']
        else:
            self.config['bandwidth'] = self.config['adc_clk'] / 2.
            self.config['center_freq'] = self.config['bandwidth'] / 2.

        self.read_str('fpga','adc_type')
        if self.config['adc_type'] == 'katadc':
            self.config['adc_demux'] = 4
            self.config['adc_n_bits'] = 8
            self.config['adc_v_scale_factor']=1/184.3
            self.config['adc_low_level_warning']=-32
            self.config['adc_high_level_warning']=0
            for input_n in range(self.config['n_inputs']):
                try:
                    self.read_int('equalisation','rf_gain_%i'%(input_n))
                except: 
                    raise RuntimeError('ERR rf_gain_%i'%(input_n))
        elif self.config['adc_type'] == 'iadc':
            self.config['adc_demux'] = 4
            self.config['adc_n_bits'] = 8
            self.config['adc_v_scale_factor']=1/368.
            self.config['adc_low_level_warning']=-35
            self.config['adc_high_level_warning']=0
        else:
            raise RuntimeError("adc_type not understood. expecting katadc or iadc.")

        self.config['fpga_clk'] = self.config['adc_clk'] / self.config['adc_demux']
        self.config['mcnt_scale_factor'] = self.config['fpga_clk']
#TODO: check this timestamp scale factor:
#        self.config['pcnt_scale_factor'] = self.config['bandwidth'] / self.config['xeng_acc_len']

        #get the receiver section:
        self.read_int('receiver','default_spead_id')
        self.read_int('receiver','rx_pkt_payload_len')
        self.config['receiver'] = dict()
        self.read_int('receiver','rx_udp_port')
        self.read_int('receiver','rx_pkt_payload_len')
        self.config['rx_udp_ip_str']=self.get_line('receiver','rx_udp_ip')
        self.config['rx_udp_ip']=struct.unpack('>I',socket.inet_aton(self.get_line('receiver','rx_udp_ip')))[0]
        self.config['rx_meta_ip_str']=self.get_line('receiver','rx_meta_ip')
        self.config['rx_meta_ip']=struct.unpack('>I',socket.inet_aton(self.get_line('receiver','rx_meta_ip')))[0]
        #print 'RX UDP IP address is %i'%self.config['rx_udp_ip']

        spead_flavour=self.get_line('receiver','spead_flavour')
        self.config['spead_flavour'] = tuple([int(i) for i in spead_flavour.split(LISTDELIMIT)])
        self.config['spead_timestamp_scale_factor']=self.config['fpga_clk']

#        if self.config['spead_flavour'][1]<(48-numpy.log2(self.config['n_chans'])): 
#            self.config['spead_timestamp_scale_factor']=(self.config['pcnt_scale_factor']/self.config['n_chans'])
#        else: 
#            self.config['spead_timestamp_scale_factor']=(int(self.config['pcnt_scale_factor'])<<int(numpy.log2(self.config['n_chans']) - (48-self.config['spead_flavour'][1])))/float(self.config['n_chans'])

        #equalisation section:

        for input_n in range(self.config['n_inputs']):
            try:
                self.read_int('equalisation','eq_gain_%i'%input_n)
            except: 
                raise RuntimeError('ERR eq_gain_%i'%(input_n))

    def write(self,section,variable,value):
        print 'Would be writing to the config file now. Mostly, this is a bad idea. Mostly. Doing nothing.'
        return
        self.config[variable] = value
        self.cp[section][variable] = str(value)
        fpw=open(self.config_file, 'w')
        print >>fpw,self.cp
        fpw.close()

    def write_var(self, filename, value):
        fp=open(VAR_RUN + '/' + filename + '.' + self.config_file_name, 'w')
        fp.write(value)
        fp.close()

    def write_var_list(self, filename, list_to_store):
        fp=open(VAR_RUN + '/' + filename + '.' + self.config_file_name, 'w')
        for v in list_to_store:
            fp.write(v + LISTDELIMIT)
        fp.close()

    def get_line(self,section,variable):
        return self.cp[section][variable]

    def read_int(self,section,variable):
        self.config[variable]=int(self.cp[section][variable])

    def read_bool(self,section,variable):
        self.config[variable]=(self.cp[section][variable] != '0')

    def read_str(self,section,variable):
        self.config[variable]=self.cp[section][variable]

    def read_float(self,section,variable):
        self.config[variable]=float(self.cp[section][variable])
