""" installer

This module defines a minimal installer for scons build scripts.  It is aimed
at *nix like systems, but I guess it could easily be adapted to other ones as
well.
"""

import fnmatch, os, os.path
import SCons.Defaults

PREFIX = "prefix"
EPREFIX = "eprefix"
BINDIR = "bindir"
LIBDIR = "libdir"
INCLUDEDIR = "includedir"
SHAREDIR = "sharedir"
PKGDIR = "pkgconfig"

def AddOptions( opts ):
        """ Adds the installer options to the opts.  """
        opts.Add( PREFIX, "Directory of architecture independant files.", "/usr" )
        opts.Add( EPREFIX, "Directory of architecture dependant files.", "${%s}" % PREFIX )
        opts.Add( BINDIR, "Directory of executables.", "${%s}/bin" % EPREFIX )
        opts.Add( LIBDIR, "Directory of libraries.", "${%s}/lib" % EPREFIX )
        opts.Add( INCLUDEDIR, "Directory of header files.", "${%s}/include" % PREFIX )
	opts.Add( PKGDIR, "Directory of pkgconfig files.", "${%s}/pkgconfig" % LIBDIR )    
	opts.Add( SHAREDIR, "Directory of script files.", "${%s}/share" % PREFIX )

def generate(env):

	class Installer:
	    """ A basic installer. """
	    def __init__( self):
	        """ Initialize the installer.
	
	        @param configuration A dictionary containing the configuration.
	        @param env The installation environment.
	        """
	        self._env = env
		self._isValid = False
	
	    def LateInit(self):
		if not self._isValid :
 			self._prefix = env.get( PREFIX, "/usr" )
	        	self._eprefix = env.get( EPREFIX, self._prefix )
	        	self._bindir = env.get( BINDIR, os.path.join( self._eprefix, "bin" ) )
	        	self._libdir = env.get( LIBDIR, os.path.join( self._eprefix, "lib" ) )
	        	self._includedir = env.get( INCLUDEDIR, os.path.join( self._prefix, "include" ) )
			self._sharedir = env.get( SHAREDIR, os.path.join( self._prefix, "share" ) )
			self._pkgdir = env.get( PKGDIR, os.path.join( self._libdir, "pkgconfig" ) )
			self._isValid = True

	    def Add( self, destdir, name, basedir="", perm=0644 ):
		#print "Adding target:", name
		self.LateInit()
	        destination = os.path.join( destdir, basedir )
	        obj = self._env.Install( destination, name )
	        self._env.Alias( "install", destination )
	        for i in obj:
	            self._env.AddPostAction( i, SCons.Defaults.Chmod( str(i), perm ) )

	    def AddProgram( self, program ):
	        """ Install a program.
	
	        @param program The program to install.
	        """
		self.LateInit()
	        self.Add( self._bindir, program, perm=0755 )
	
	    def AddLibrary( self, library ):
	        """ Install a library.
	
	        @param library the library to install.
	        """
		self.LateInit()
	        self.Add( self._libdir, library, perm=0755 )

	    def AddPkgconfig( self, library ):
	        """ Install a Pkgconfig file.
	
	        @param the path to the pkgconfig root.
	        """
		self.LateInit()
	        self.Add( self._pkgdir, library )

	    def AddShare( self, header, basedir="" ):
		LateInit()	
	        self.Add( self._sharedir, header, basedir )

	    def AddShares( self, parent , pattern, basedir="", recursive=False):
		
	        for entry in os.listdir( parent ):
	            entrypath = os.path.join( parent, entry )
	            if os.path.isfile( entrypath ) and  \
			   fnmatch.fnmatch( entry, pattern ):
	                self.AddShare( entrypath, basedir )
	            elif os.path.isdir( entrypath ) and recursive:
	                self.AddShares( entrypath, pattern,
					 os.path.join( basedir, entry ),
					 recursive )


	    def AddHeader( self, header, basedir="" ):
		self.LateInit()
	        self.Add( self._includedir, header, basedir )
	
	    def AddHeaders( self, parent, pattern, basedir="", recursive=False ):
	        """ Installs a set of headers.
	
	        @param parent The parent directory of the headers.
	        @param pattern A pattern to identify the files that are headers.
	        @param basedir The subdirectory in which to install the headers.
	        @param recursive Search recursively for headers.
	        """
	        for entry in os.listdir( parent ):
	            entrypath = os.path.join( parent, entry )
		    if os.path.isfile( entrypath ) and  \
			   fnmatch.fnmatch( entry, pattern ):
	                self.AddHeader( entrypath, basedir )
	            elif os.path.isdir( entrypath ) and recursive:
	                self.AddHeaders( entrypath, pattern,
					 os.path.join( basedir, entry ),
					 recursive )
	
	env.Installer = Installer
	env.AddInstallerOptions  = AddOptions
	AddOptions( env.Option )	
	env.installer = Installer()
 	

def exists(env):
	return True


