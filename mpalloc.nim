##
##  Copyright (c) 2008-2020 Stefan Krah. All rights reserved.
##
##  Redistribution and use in source and binary forms, with or without
##  modification, are permitted provided that the following conditions
##  are met:
##
##  1. Redistributions of source code must retain the above copyright
##     notice, this list of conditions and the following disclaimer.
##
##  2. Redistributions in binary form must reproduce the above copyright
##     notice, this list of conditions and the following disclaimer in the
##     documentation and/or other materials provided with the distribution.
##
##  THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS "AS IS" AND
##  ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
##  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
##  ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE
##  FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
##  DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
##  OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
##  HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
##  LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
##  OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
##  SUCH DAMAGE.
##

import
  mpdecimal

##  Internal header file: all symbols have local scope in the DSO
##  MPD_PRAGMA(MPD_HIDE_SYMBOLS_START)

proc mpdSwitchToDyn*(result: ptr MpdT; nwords: MpdSsizeT; status: ptr uint32T): cint {.
    importc: "mpd_switch_to_dyn", header: "mpalloc.h".}
proc mpdSwitchToDynZero*(result: ptr MpdT; nwords: MpdSsizeT; status: ptr uint32T): cint {.
    importc: "mpd_switch_to_dyn_zero", header: "mpalloc.h".}
proc mpdReallocDyn*(result: ptr MpdT; nwords: MpdSsizeT; status: ptr uint32T): cint {.
    importc: "mpd_realloc_dyn", header: "mpalloc.h".}
proc mpdSwitchToDynCxx*(result: ptr MpdT; nwords: MpdSsizeT): cint {.
    importc: "mpd_switch_to_dyn_cxx", header: "mpalloc.h".}
proc mpdReallocDynCxx*(result: ptr MpdT; nwords: MpdSsizeT): cint {.
    importc: "mpd_realloc_dyn_cxx", header: "mpalloc.h".}
## MPD_PRAGMA(MPD_HIDE_SYMBOLS_END) /* restore previous scope rules */
