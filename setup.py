from distutils.core import setup, Extension
import os, sys, glob

__version__ = '0.0.1'

setup(name = 'afro',
    version = __version__,
    description = 'Interfaces to an Astronomically Fast Raw Output SPEAD streamer device',
    long_description = 'Provides interfaces and functions to configure a raw SPEAD streamer.',
    license = 'GPL',
    author = 'Jason Manley',
    author_email = 'jason_manley at ska.ac.za',
    url = 'https://github.com/ska-sa/afro',
    classifiers=[
        'Development Status :: 3 - Alpha',
        'Intended Audience :: Developers',
        'Operating System :: OS Independent',
        'License :: OSI Approved :: GNU General Public License (GPL)',
        'Topic :: Scientific/Engineering :: Astronomy',
        'Topic :: Software Development :: Libraries :: Python Modules',
        ],
    requires=['katcp', 'pylab','matplotlib','iniparse', 'numpy', 'spead', 'curses', 'construct','corr'],
    provides=['afro'],
    package_dir = {'afro':'src'},
    packages = ['afro'],
    scripts=glob.glob('scripts/*'),
    data_files=[('/etc/afro',['etc/default']),
                #('/var/run/corr',['support_files/sync_time'])
                ]
)

