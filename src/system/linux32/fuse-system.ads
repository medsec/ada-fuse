-- Architecture dependent declarations

with Interfaces;
with Interfaces.C;
with Ada.Unchecked_Conversion;

generic

   type User_Data_Type is private;
   type File is limited private;
   type File_Access is access File;

package Fuse.System is

   -- Types

   subtype Dev_T         is Interfaces.Unsigned_64;
   subtype Ino_T         is Interfaces.Unsigned_64;
   subtype Mode_T        is Interfaces.Unsigned_32;
   subtype NLink_T       is Interfaces.Unsigned_32;
   subtype UID_T         is Interfaces.Unsigned_32;
   subtype GID_T         is Interfaces.Unsigned_32;
   subtype PID_T         is Interfaces.Integer_32;
   subtype Off_T         is Interfaces.Integer_64;
   subtype Time_T        is Interfaces.Integer_32;
   subtype BlkSize_T     is Interfaces.Integer_32;
   subtype BlkCnt_T      is Interfaces.Integer_64;
   subtype SSize_T       is Interfaces.Integer_32;


   -- Context

   type Context_Type is
      record
         UID   : UID_T;
         GID   : GID_T;
         PID   : PID_T;
         Data  : access User_Data_Type;
      end record;

   type Context_Access is access all Context_Type;

   pragma Convention (C, Context_Type);
   for Context_Type'Size use 24*8;

   for Context_Type use
      record
         UID       at   4 range 0 .. 31;
         GID       at   8 range 0 .. 31;
         PID       at  12 range 0 .. 31;
         Data      at  16 range 0 .. 31;
      end record;

   pragma Convention (C, Context_Access);


   -- File Info Flags

   type RW_Type is
      (O_RDONLY, O_WRONLY, O_RDWR, O_ACCMODE);

   for RW_Type use (
      O_RDONLY  => 0,
      O_WRONLY  => 1,
      O_RDWR    => 2,
      O_ACCMODE => 3);

   type Flags_Type(Option : Boolean := True) is
      record case Option is
         when True =>
            RW          : RW_Type;
            O_CREAT     : Boolean;
            O_EXCL      : Boolean;
            O_NOCTTY    : Boolean;
            O_TRUNC     : Boolean;
            O_APPEND    : Boolean;
            O_NONBLOCK  : Boolean;
            --O_SYNC      : Boolean;
            --O_FSYNC     : Boolean;
            O_ASYNC     : Boolean;
            O_DSYNC     : Boolean;
            --O_RSYNC     : Boolean;
            O_DIRECTORY : Boolean;
            O_NOFOLLOW  : Boolean;
            O_CLOEXEC   : Boolean;
            when False =>
               O_NDELAY    : Boolean;
            end case;
      end record;

   pragma Unchecked_Union(Flags_Type);
   pragma Convention (C, Flags_Type);

   pragma Warnings (Off, "no component clause given for ""Option""*");
   for Flags_Type use
      record
         RW          at 0 range  0.. 1; --       8#3#
         O_CREAT     at 0 range  6.. 6; --     8#100#
         O_EXCL      at 0 range  7.. 7; --     8#200#
         O_NOCTTY    at 0 range  8.. 8; --     8#400#
         O_TRUNC     at 0 range  9.. 9; --    8#1000#
         O_APPEND    at 0 range 10..10; --    8#2000#
         O_NONBLOCK  at 0 range 11..11; --    8#4000#
         O_NDELAY    at 0 range 11..11; --    8#4000# 
         -- Unchecked_Union allows to store 
         -- O_NONBLOCK and O_NDELAY at the same bit position
       --O_SYNC                         -- 8#4010000#
       --O_FSYNC = O_SYNC               -- 8#4010000#
         -- We don't know how to represent those.
         O_ASYNC     at 0 range 13..13; --   8#20000#
         O_DSYNC     at 0 range 12..12; --   8#10000#
       --O_RSYNC = O_SYNC               -- 8#4010000#
         O_DIRECTORY at 0 range 16..16; --  8#200000#
         O_NOFOLLOW  at 0 range 17..17; --  8#400000#
         O_CLOEXEC   at 0 range 19..19; -- 8#2000000#
      end record;
   pragma Warnings (On, "no component clause given for ""Option""*");

   for Flags_Type'Size use Interfaces.C.int'Size;


   -- File_Info

   type File_Info_Type is record
      Flags       : Flags_Type;
      Fh_Old      : Interfaces.C.unsigned_long;
      Writepage   : Interfaces.C.int;
      Direct_IO   : Boolean := True;
      Keep_Cache  : Boolean := True;
      Flush       : Boolean := True;
      Nonseekable : Boolean := True;
      Fh          : File_Access;
      Lock_Owner  : Interfaces.Unsigned_64;
   end record;

   type File_Info_Access is access File_Info_Type;

   pragma Convention (C, File_Info_Type);
   for File_Info_Type'Size use 32*8;

   for File_Info_Type use
      record
         Flags       at   0 range 0 .. 31;
         Fh_Old      at   4 range 0 .. 31;
         Writepage   at   8 range 0 .. 31;
         Direct_IO   at  12 range 0 ..  0;
         Keep_Cache  at  12 range 1 ..  1;
         Flush       at  12 range 2 ..  2;
         Nonseekable at  12 range 3 ..  3;
         Fh          at  16 range 0 .. 63;
         Lock_Owner  at  24 range 0 .. 63;
      end record;


   -- St_Mode

   type St_Mode_Type is
      record
         S_IFIFO : Boolean;
         S_IFCHR : Boolean;
         S_IFDIR : Boolean;
         S_IFREG : Boolean;

         S_IRUSR : Boolean;
         S_IWUSR : Boolean;
         S_IXUSR : Boolean;

         S_IRGRP : Boolean;
         S_IWGRP : Boolean;
         S_IXGRP : Boolean;

         S_IROTH : Boolean;
         S_IWOTH : Boolean;
         S_IXOTH : Boolean;

         S_ISUID : Boolean;
         S_ISGID : Boolean;
         S_ISVTX : Boolean;
      end record;

   for St_Mode_Type use
      record
         S_IFIFO      at 0 range 12..12; --    8#10000#
         S_IFCHR      at 0 range 13..13; --    8#20000#
         S_IFDIR      at 0 range 14..14; --    8#40000#
         S_IFREG      at 0 range 15..15; --   8#100000#

         S_IRUSR      at 0 range  8.. 8; --      8#400#
         S_IWUSR      at 0 range  7.. 7; --      8#200#
         S_IXUSR      at 0 range  6.. 6; --      8#100#

         S_IRGRP      at 0 range  5.. 5; --       8#40#
         S_IWGRP      at 0 range  4.. 4; --       8#20#
         S_IXGRP      at 0 range  3.. 3; --       8#10#

         S_IROTH      at 0 range  2.. 2; --        8#4#
         S_IWOTH      at 0 range  1.. 1; --        8#2#
         S_IXOTH      at 0 range  0.. 0; --        8#1#

         S_ISUID      at 0 range 11..11; --     8#4000#
         S_ISGID      at 0 range 10..10; --     8#2000#
         S_ISVTX      at 0 range  9.. 9; --     8#1000#
      end record;

   for St_Mode_Type'Size use Mode_T'Size;

   S_IFIFO : constant St_Mode_Type := (S_IFIFO => true, others => false);
   S_IFCHR : constant St_Mode_Type := (S_IFCHR => true, others => false);
   S_IFDIR : constant St_Mode_Type := (S_IFDIR => true, others => false);
   S_IFREG : constant St_Mode_Type := (S_IFREG => true, others => false);
   S_IRUSR : constant St_Mode_Type := (S_IRUSR => true, others => false);
   S_IWUSR : constant St_Mode_Type := (S_IWUSR => true, others => false);
   S_IXUSR : constant St_Mode_Type := (S_IXUSR => true, others => false);
   S_IRGRP : constant St_Mode_Type := (S_IRGRP => true, others => false);
   S_IWGRP : constant St_Mode_Type := (S_IWGRP => true, others => false);
   S_IXGRP : constant St_Mode_Type := (S_IXGRP => true, others => false);
   S_IROTH : constant St_Mode_Type := (S_IROTH => true, others => false);
   S_IWOTH : constant St_Mode_Type := (S_IWOTH => true, others => false);
   S_IXOTH : constant St_Mode_Type := (S_IXOTH => true, others => false);
   S_ISUID : constant St_Mode_Type := (S_ISUID => true, others => false);
   S_ISGID : constant St_Mode_Type := (S_ISGID => true, others => false);
   S_ISVTX : constant St_Mode_Type := (S_ISVTX => true, others => false);

   S_IFMT : constant St_Mode_Type :=                         -- 8#170000#
     (S_IFIFO | S_IFCHR | S_IFDIR | S_IFREG => true, others => false);

   S_IFBLK: constant St_Mode_Type :=                         --  8#60000#
     (S_IFCHR | S_IFDIR => true, others => false);

   S_IFLNK : constant St_Mode_Type :=                        -- 8#120000#
     (S_IFCHR | S_IFREG => true, others => false);

   S_IFSOCK : constant St_Mode_Type :=                       -- 8#140000#
     (S_IFDIR | S_IFREG => true, others => false);

   S_IRWXU : constant St_Mode_Type :=                        --    8#700#
     (S_IRUSR | S_IWUSR | S_IXUSR => true, others => false);

   S_IRWXG : constant St_Mode_Type :=                        --     8#70#
     (S_IRGRP | S_IWGRP | S_IXGRP => true, others => false);

   S_IRWXO : constant St_Mode_Type :=                        --      8#7#
     (S_IROTH | S_IWOTH | S_IXOTH => true, others => false);

   function Mode_T_to_St_Mode is new Ada.Unchecked_Conversion
     (Source => Mode_T,
      Target => St_Mode_Type);

   function St_Mode_to_Mode_T is new Ada.Unchecked_Conversion
     (Source => St_Mode_Type,
      Target => Mode_T);

   function "or"
     (Left   : St_Mode_Type;
      Right  : St_Mode_Type) return St_Mode_Type;

   function "or"
     (Left   : St_Mode_Type;
      Right  : Mode_T) return St_Mode_Type;

   function "or"
     (Left   : Mode_T;
      Right  : St_Mode_Type) return St_Mode_Type;


   -- Stat

   type Stat_Type is
      record
         St_Dev     : Dev_T;
         -- ID of device containing file
         St_Ino     : Ino_T;
         -- file serial number
         St_Mode    : St_Mode_Type;
         -- permissions and file type
         St_Nlink   : NLink_T;
         -- number of links to the file
         St_Uid     : Uid_T;
         -- user ID of file
         St_Gid     : Gid_T;
         -- group ID of file
         St_Rdev    : Dev_T;
         -- device ID (if file is character or block special)
         St_Size    : Off_T;
         -- file size in bytes (if file is a regular file)
         St_Atime   : Time_T;
         -- time of last access
         St_Mtime   : Time_T;
         -- time of last data modification
         St_Ctime   : Time_T;
         -- time of last status change
         St_Blksize : Blksize_T;
         -- a filesystem-specific preferred I/O block size for this object.
         -- In some filesystem types, this may vary from file to file
         St_Blocks  : Blkcnt_T;
         -- number of blocks allocated for this object
      end record;

   pragma Convention (C, Stat_Type);

   for Stat_Type use
      record
         St_Dev      at   0 range  0 .. 63;
         St_Ino      at  88 range  0 .. 63;
         St_Mode     at  16 range  0 .. 31;
         St_Nlink    at  20 range  0 .. 31;
         St_Uid      at  24 range  0 .. 31;
         St_Gid      at  28 range  0 .. 31;
         St_Rdev     at  32 range  0 .. 63;
         St_Size     at  44 range  0 .. 63;
         St_Atime    at  64 range  0 .. 31;
         St_Mtime    at  72 range  0 .. 31;
         St_Ctime    at  80 range  0 .. 31;
         St_Blksize  at  52 range  0 .. 31;
         St_Blocks   at  56 range  0 .. 63;
      end record;

   for Stat_Type'Size use 96*8;

   type Stat_Access is access all Stat_Type;

   Null_Stat : constant Stat_Type :=
     (St_Dev => 0,
      St_Ino => 0,
      St_Mode => (others => False),
      St_Nlink => 0,
      St_Uid => 0,
      St_Gid => 0,
      St_Rdev => 0,
      St_Size => 0,
      St_Atime => 0,
      St_Mtime => 0,
      St_Ctime => 0,
      St_Blksize => 0,
      St_Blocks => 0);


   -- StatVFS

   type StatVFS_Type is null record;
   -- TODO import statvfs from /usr/include/bits/statvfs.h
   -- Already works because we only pass it to the C function yet.
   -- For an own implementations of StatFS a representation clause
   -- is needed.

   type StatVFS_Access is access all StatVFS_Type;


   --UTime

   type UTimeBuffer_Type is null record;
   -- TODO import UTimBuf
   -- same as StatVFS

   type UTimeBuffer_Access is access all UTimeBuffer_Type;


   --Init

   type Fuse_Conn_Info_Type is null record;
   -- TODO import fuse_conn_info from fuse_common.h

   type Fuse_Conn_Info_Access is access all Fuse_Conn_Info_Type;


   -- Errors

   type Error_Type is (EXIT_SUCCESS, EPERM, ENOENT, ESRCH, EINTR, EIO,
      ENXIO, E2BIG, ENOEXEC, EBADF, ECHILD, EAGAIN, ENOMEM, EACCES,
      EFAULT, ENOTBLK, EBUSY, EEXIST, EXDEV, ENODEV, ENOTDIR, EISDIR,
      EINVAL, ENFILE, EMFILE, ENOTTY, ETXTBSY, EFBIG, ENOSPC, ESPIPE,
      EROFS, EMLINK, EPIPE, EDOM, ERANGE, EDEADLK, ENAMETOOLONG,
      ENOLCK, ENOSYS, ENOTEMPTY, ELOOP, ENOMSG, EIDRM, ECHRNG,
      EL2NSYNC, EL3HLT, EL3RST, ELNRNG, EUNATCH, ENOCSI, EL2HLT,
      EBADE, EBADR, EXFULL, ENOANO, EBADRQC, EBADSLT, EBFONT, ENOSTR,
      ENODATA, ETIME, ENOSR, ENONET, ENOPKG, EREMOTE, ENOLINK, EADV,
      ESRMNT, ECOMM, EPROTO, EMULTIHOP, EDOTDOT, EBADMSG, EOVERFLOW,
      ENOTUNIQ, EBADFD, EREMCHG, ELIBACC, ELIBBAD, ELIBSCN, ELIBMAX,
      ELIBEXEC, EILSEQ, ERESTART, ESTRPIPE, EUSERS, ENOTSOCK,
      EDESTADDRREQ, EMSGSIZE, EPROTOTYPE, ENOPROTOOPT,
      EPROTONOSUPPORT, ESOCKTNOSUPPORT, EOPNOTSUPP, EPFNOSUPPORT,
      EAFNOSUPPORT, EADDRINUSE, EADDRNOTAVAIL, ENETDOWN, ENETUNREACH,
      ENETRESET, ECONNABORTED, ECONNRESET, ENOBUFS, EISCONN, ENOTCONN,
      ESHUTDOWN, ETOOMANYREFS, ETIMEDOUT, ECONNREFUSED, EHOSTDOWN,
      EHOSTUNREACH, EALREADY, EINPROGRESS, ESTALE, EUCLEAN, ENOTNAM,
      ENAVAIL, EISNAM, EREMOTEIO, EDQUOT, ENOMEDIUM, EMEDIUMTYPE,
      ECANCELED, ENOKEY, EKEYEXPIRED, EKEYREVOKED, EKEYREJECTED,
      EOWNERDEAD, ENOTRECOVERABLE, ERFKILL, EHWPOISON);

--   for Error_Type'Size use Interfaces.C.int'Size;
   --  could become neccessary for reading errno
   --  see Aux.Get_Error;

   for Error_Type use (
      EXIT_SUCCESS     =>   0,
      EPERM            =>   1,  -- Operation not permitted
      ENOENT           =>   2,  -- No such file or directory
      ESRCH            =>   3,  -- No such process
      EINTR            =>   4,  -- Interrupted system call
      EIO              =>   5,  -- I/O error
      ENXIO            =>   6,  -- No such device or address
      E2BIG            =>   7,  -- Argument list too long
      ENOEXEC          =>   8,  -- Exec format error
      EBADF            =>   9,  -- Bad file number
      ECHILD           =>  10,  -- No child processes
      EAGAIN           =>  11,  -- Try again
      ENOMEM           =>  12,  -- Out of memory
      EACCES           =>  13,  -- Permission denied
      EFAULT           =>  14,  -- Bad address
      ENOTBLK          =>  15,  -- Block device required
      EBUSY            =>  16,  -- Device or resource busy
      EEXIST           =>  17,  -- File exists
      EXDEV            =>  18,  -- Cross-device link
      ENODEV           =>  19,  -- No such device
      ENOTDIR          =>  20,  -- Not a directory
      EISDIR           =>  21,  -- Is a directory
      EINVAL           =>  22,  -- Invalid argument
      ENFILE           =>  23,  -- File table overflow
      EMFILE           =>  24,  -- Too many open files
      ENOTTY           =>  25,  -- Not a typewriter
      ETXTBSY          =>  26,  -- Text file busy
      EFBIG            =>  27,  -- File too large
      ENOSPC           =>  28,  -- No space left on device
      ESPIPE           =>  29,  -- Illegal seek
      EROFS            =>  30,  -- Read-only file system
      EMLINK           =>  31,  -- Too many links
      EPIPE            =>  32,  -- Broken pipe
      EDOM             =>  33,  -- Math argument out of domain of func
      ERANGE           =>  34,  -- Math result not representable
      EDEADLK          =>  35,  -- Resource deadlock would occur
      ENAMETOOLONG     =>  36,  -- File name too long
      ENOLCK           =>  37,  -- No record locks available
      ENOSYS           =>  38,  -- Function not implemented
      ENOTEMPTY        =>  39,  -- Directory not empty
      ELOOP            =>  40,  -- Too many symbolic links encountered
      ENOMSG           =>  42,  -- No message of desired type
      EIDRM            =>  43,  -- Identifier removed
      ECHRNG           =>  44,  -- Channel number out of range
      EL2NSYNC         =>  45,  -- Level 2 not synchronized
      EL3HLT           =>  46,  -- Level 3 halted
      EL3RST           =>  47,  -- Level 3 reset
      ELNRNG           =>  48,  -- Link number out of range
      EUNATCH          =>  49,  -- Protocol driver not attached
      ENOCSI           =>  50,  -- No CSI structure available
      EL2HLT           =>  51,  -- Level 2 halted
      EBADE            =>  52,  -- Invalid exchange
      EBADR            =>  53,  -- Invalid request descriptor
      EXFULL           =>  54,  -- Exchange full
      ENOANO           =>  55,  -- No anode
      EBADRQC          =>  56,  -- Invalid request code
      EBADSLT          =>  57,  -- Invalid slot
      EBFONT           =>  59,  -- Bad font file format
      ENOSTR           =>  60,  -- Device not a stream
      ENODATA          =>  61,  -- No data available
      ETIME            =>  62,  -- Timer expired
      ENOSR            =>  63,  -- Out of streams resources
      ENONET           =>  64,  -- Machine is not on the network
      ENOPKG           =>  65,  -- Package not installed
      EREMOTE          =>  66,  -- Object is remote
      ENOLINK          =>  67,  -- Link has been severed
      EADV             =>  68,  -- Advertise error
      ESRMNT           =>  69,  -- Srmount error
      ECOMM            =>  70,  -- Communication error on send
      EPROTO           =>  71,  -- Protocol error
      EMULTIHOP        =>  72,  -- Multihop attempted
      EDOTDOT          =>  73,  -- RFS specific error
      EBADMSG          =>  74,  -- Not a data message
      EOVERFLOW        =>  75,  -- Value too large for defined data type
      ENOTUNIQ         =>  76,  -- Name not unique on network
      EBADFD           =>  77,  -- File descriptor in bad state
      EREMCHG          =>  78,  -- Remote address changed
      ELIBACC          =>  79,  -- Can not access a needed shared library
      ELIBBAD          =>  80,  -- Accessing a corrupted shared library
      ELIBSCN          =>  81,  -- .lib section in a.out corrupted
      ELIBMAX          =>  82,  -- Attempting to link in too many shared libraries
      ELIBEXEC         =>  83,  -- Cannot exec a shared library directly
      EILSEQ           =>  84,  -- Illegal byte sequence
      ERESTART         =>  85,  -- Interrupted system call should be restarted
      ESTRPIPE         =>  86,  -- Streams pipe error
      EUSERS           =>  87,  -- Too many users
      ENOTSOCK         =>  88,  -- Socket operation on non-socket
      EDESTADDRREQ     =>  89,  -- Destination address required
      EMSGSIZE         =>  90,  -- Message too long
      EPROTOTYPE       =>  91,  -- Protocol wrong type for socket
      ENOPROTOOPT      =>  92,  -- Protocol not available
      EPROTONOSUPPORT  =>  93,  -- Protocol not supported
      ESOCKTNOSUPPORT  =>  94,  -- Socket type not supported
      EOPNOTSUPP       =>  95,  -- Operation not supported on transport endpoint
      EPFNOSUPPORT     =>  96,  -- Protocol family not supported
      EAFNOSUPPORT     =>  97,  -- Address family not supported by protocol
      EADDRINUSE       =>  98,  -- Address already in use
      EADDRNOTAVAIL    =>  99,  -- Cannot assign requested address
      ENETDOWN         =>  100, -- Network is down
      ENETUNREACH      =>  101, -- Network is unreachable
      ENETRESET        =>  102, -- Network dropped connection because of reset
      ECONNABORTED     =>  103, -- Software caused connection abort
      ECONNRESET       =>  104, -- Connection reset by peer
      ENOBUFS          =>  105, -- No buffer space available
      EISCONN          =>  106, -- Transport endpoint is already connected
      ENOTCONN         =>  107, -- Transport endpoint is not connected
      ESHUTDOWN        =>  108, -- Cannot send after transport endpoint shutdown
      ETOOMANYREFS     =>  109, -- Too many references: cannot splice
      ETIMEDOUT        =>  110, -- Connection timed out
      ECONNREFUSED     =>  111, -- Connection refused
      EHOSTDOWN        =>  112, -- Host is down
      EHOSTUNREACH     =>  113, -- No route to host
      EALREADY         =>  114, -- Operation already in progress
      EINPROGRESS      =>  115, -- Operation now in progress
      ESTALE           =>  116, -- Stale NFS file handle
      EUCLEAN          =>  117, -- Structure needs cleaning
      ENOTNAM          =>  118, -- Not a XENIX named type file
      ENAVAIL          =>  119, -- No XENIX semaphores available
      EISNAM           =>  120, -- Is a named type file
      EREMOTEIO        =>  121, -- Remote I/O error
      EDQUOT           =>  122, -- Quota exceeded
      ENOMEDIUM        =>  123, -- No medium found
      EMEDIUMTYPE      =>  124, -- Wrong medium type
      ECANCELED        =>  125, -- Operation Canceled
      ENOKEY           =>  126, -- Required key not available
      EKEYEXPIRED      =>  127, -- Key has expired
      EKEYREVOKED      =>  128, -- Key has been revoked
      EKEYREJECTED     =>  129, -- Key was rejected by service
      EOWNERDEAD       =>  130, -- Owner died
      ENOTRECOVERABLE  =>  131, -- State not recoverable
      ERFKILL          =>  132, -- Operation not possible due to RF-kill
      EHWPOISON        =>  133);-- Memory page has hardware error

   EWOULDBLOCK : constant Error_Type := EAGAIN; -- Operation would block
   EDEADLOCK   : constant Error_Type := EDEADLK;


   -- Operations

   generic
      type GetAttr_C_Type is private;
      type ReadLink_C_Type is private;
      type MkNod_C_Type is private;
      type MkDir_C_Type is private;
      type Unlink_C_Type is private;
      type RmDir_C_Type is private;
      type SymLink_C_Type is private;
      type Rename_C_Type is private;
      type Link_C_Type is private;
      type ChMod_C_Type is private;
      type ChOwn_C_Type is private;
      type Truncate_C_Type is private;
      type UTime_C_Type is private;
      type Open_C_Type is private;
      type Read_C_Type is private;
      type Write_C_Type is private;
      type StatFS_C_Type is private;
      type Flush_C_Type is private;
      type Release_C_Type is private;
      type FSync_C_Type is private;
      type SetXAttr_C_Type is private;
      type GetXAttr_C_Type is private;
      type ListXAttr_C_Type is private;
      type RemoveXAttr_C_Type is private;
      type OpenDir_C_Type is private;
      type ReadDir_C_Type is private;
      type ReleaseDir_C_Type is private;
      type FSyncDir_C_Type is private;
      type Init_C_Type is private;
      type Destroy_C_Type is private;
      type Access_C_Type is private;
      type Create_C_Type is private;
      type FTruncate_C_Type is private;
      type FGetAttr_C_Type is private;
--      type Lock_C_Type is private;
--      type UTimeNS_C_Type is private;
--      type BMap_C_Type is private;
--      type IOCtl_C_Type is private;
--      type Poll_C_Type is private;

   package Operations is

      type Operations_C_Record is
         record
            GetAttr_C     : GetAttr_C_Type;
            Readlink_C    : Readlink_C_Type;
            Mknod_C       : Mknod_C_Type;
            Mkdir_C       : Mkdir_C_Type;
            Unlink_C      : Unlink_C_Type;
            RmDir_C       : RmDir_C_Type;
            Symlink_C     : Symlink_C_Type;
            Rename_C      : Rename_C_Type;
            Link_C        : Link_C_Type;
            Chmod_C       : Chmod_C_Type;
            Chown_C       : Chown_C_Type;
            Truncate_C    : Truncate_C_Type;
            Utime_C       : Utime_C_Type;
            Open_C        : Open_C_Type;
            Read_C        : Read_C_Type;
            Write_C       : Write_C_Type;
            Statfs_C      : Statfs_C_Type;
            Flush_C       : Flush_C_Type;
            Release_C     : Release_C_Type;
            Fsync_C       : Fsync_C_Type;
            Setxattr_C    : Setxattr_C_Type;
            Getxattr_C    : Getxattr_C_Type;
            Listxattr_C   : Listxattr_C_Type;
            Removexattr_C : Removexattr_C_Type;
            OpenDir_C     : OpenDir_C_Type;
            ReadDir_C     : ReadDir_C_Type;
            ReleaseDir_C  : ReleaseDir_C_Type;
            FSyncDir_C    : FSyncDir_C_Type;
            Init_C        : Init_C_Type;
            Destroy_C     : Destroy_C_Type;
            Access_C      : Access_C_Type;
            Create_C      : Create_C_Type;
            FTruncate_C   : FTruncate_C_Type;
            FGetAttr_C    : FGetAttr_C_Type;
   --         --Lock_C        : Lock_C_Type;
   --         --UTimens_C     : UTimens_C_Type;
   --         --BMap_C        : BMap_C_Type;
            Flag_Nullpath_Ok : Boolean;
   --         --IOctl_C       : IOctl_C_Type;
   --         --Poll_C        : Poll_C_Type;
         end record;


      for Operations_C_Record use
         record
            GetAttr_C     at   0 range 0..31;
            Readlink_C    at   4 range 0..31;
            Mknod_C       at  12 range 0..31;
            Mkdir_C       at  24 range 0..31;
            Unlink_C      at  16 range 0..31;
            RmDir_C       at  20 range 0..31;
            Symlink_C     at  28 range 0..31;
            Rename_C      at  32 range 0..31;
            Link_C        at  36 range 0..31;
            Chmod_C       at  40 range 0..31;
            Chown_C       at  44 range 0..31;
            Truncate_C    at  48 range 0..31;
            Utime_C       at  52 range 0..31;
            Open_C        at  56 range 0..31;
            Read_C        at  60 range 0..31;
            Write_C       at  64 range 0..31;
            Statfs_C      at  68 range 0..31;
            Flush_C       at  72 range 0..31;
            Release_C     at  76 range 0..31;
            Fsync_C       at  80 range 0..31;
            Setxattr_C    at  84 range 0..31;
            Getxattr_C    at  88 range 0..31;
            Listxattr_C   at  92 range 0..31;
            Removexattr_C at  96 range 0..31;
            OpenDir_C     at 100 range 0..31;
            ReadDir_C     at 104 range 0..31;
            ReleaseDir_C  at 108 range 0..31;
            FSyncDir_C    at 112 range 0..31;
            Init_C        at 116 range 0..31;
            Destroy_C     at 120 range 0..31;
            Access_C      at 124 range 0..31;
            Create_C      at 128 range 0..31;
            FTruncate_C   at 132 range 0..31;
            FGetAttr_C    at 136 range 0..31;
--            --Lock_C        at 140 range 0..31;
--            --UTimens_C     at 144 range 0..31;
--            --BMap_C        at 148 range 0..31;
            Flag_Nullpath_Ok at 152 range 0..0;
--            --IOctl_C       at 156 range 0..31;
--            --Poll_C        at 160 range 0..31;
         end record;

      for Operations_C_Record'Size use 164*8;
      pragma Convention (C, Operations_C_Record);
      type Operations_C_Access is access all Operations_C_Record;

   end Operations;

end Fuse.System;

-- vim: ts=3 sw=3 et
