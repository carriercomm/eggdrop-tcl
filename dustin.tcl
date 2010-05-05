#
# dustin.tcl - contains most of the botnet logic and all most commands
#              to manipulate the bot's behavior via the partyline.

# [float] if lag for any bot is bigger than N, do not include it in procedures.
set dustin_lag_limit "3"

proc dustin_check_ops {n u h c m v} {
  global botnick relay
  if {$v != $botnick} {
    return
  }
  foreach x [stdio_randlist [chanlist $c]] {
    foreach b [bots] { 
      if {[relay $b "botnick"] == $x && ![isop $x $c] && [matchattr $b o|o $c] && ![matchattr $b d|d $c]} {
        stdio_cookie_op $x $c
        return $x
      }
    }
  }
}
 
proc dustin_needed {c t} {
  global botnick relay botname
  switch -exact $t {
    "op" {
      foreach x [stdio_randlist [chanlist $c]] {
        foreach b [bots] { 
          if {$x != $botnick && [relay $b "botnick"] == $x && [isop $x $c]} {
            putbot $b "dustin op_wantops $c $botnick"
            return
          }
        }
      }
    }
    "unban" {
      putallbots "dustin op_wantunban $c $botname $botnick"
    }
    "invite" {
      putallbots "dustin op_wantinvite $c $botnick"
    }
    "limit" {
      putallbots "dustin op_wantlimit $c $botnick"
    }
    "key" {
      putallbots "dustin op_wantkey $c"
    }
  }
}
proc dustin_dcc_njoin {h i arg} {
  stdio_putcmd $h $arg
  global dustin_control lastbind
  if {![info exists dustin_control($h)]} {
    set dustin_control($h) *
  }
  set c [lindex $arg 0]
  set k [lindex $arg 1]
  if {![regexp {^[#\+&!]} $c]} {
    putidx $i "syntax: $lastbind <channel> \[key\]"
    return
  }
  dustin_sendcmd $dustin_control($h) "njoin $c $k"
  if {[dustin_control $dustin_control($h)]} {
    if {![validchan $c]} {
      channel add $c
      if {$k != ""} {
        putquick "JOIN $c $k"
      }
    }
  }
}
proc dustin_dcc_npart {h i arg} {
  stdio_putcmd $h $arg
  global dustin_control lastbind
  if {![info exists dustin_control($h)]} {
    set dustin_control($h) *
  }
  set c [lindex $arg 0]
  if {![regexp {^[#\+&!]} $c]} {
    putidx $i "syntax: $lastbind <channel> \[key\]"
    return
  }
  dustin_sendcmd $dustin_control($h) "npart $c"
  if {[dustin_control $dustin_control($h)]} {
    if {[validchan $c] && [isdynamic $c]} {
      channel remove $c
    }
  }
}
proc dustin_dcc_nshell {h i arg} {
  global botnet-nick dustin_control lastbind relay
  stdio_putcmd $h $arg
  if {![info exists dustin_control($h)]} {
    set dustin_control($h) *
  }
  set b [lindex $arg 0]
  if {$b != "" && ![islinked $b]} {
    putidx $i "syntax: $lastbind \[bot\]"
    return
  }
  if {$b != ""} {
    putidx $i "$b: [relay $b "hostname"], [relay $b "unames"], [relay $b "uptime"]"
    return
  }
  if {[dustin_control $dustin_control($h)]} {
    catch {exec uptime} u
    regsub -all {\s{2}} $u { } u
    putidx $i "${botnet-nick}: [info hostname], [unames], [lrange $u 1 end]"
  }
  foreach b [dustin_bots $dustin_control($h)] {
    putidx $i "$b: [relay $b "hostname"], [relay $b "unames"], [relay $b "uptime"]"
  }
}
proc dustin_dcc_ncontrol {h i arg} {
  global lastbind dustin_control
  stdio_putcmd $h $arg
  if {$arg == ""} {
    putidx $i "syntax: $lastbind <bot>\[,bot2,bot3,...\]"
    return
  }
  regsub -all {\s} $arg {,} arg
  regsub -all {,,} $arg {,} arg
  set dustin_control($h) $arg
  putidx $i "now controlling bots: $arg"
}  
proc dustin_dcc_ntraffic {h i arg} {
  global botnet-nick dustin_control lastbind relay
  stdio_putcmd $h $arg
  if {![info exists dustin_control($h)]} {
    set dustin_control($h) *
  }
  if {[llength $arg] == "2"} {
    set b [lindex $arg 0]
    set k [lindex $arg 1]
    if {![regexp {^(?:irc|botnet|partyline|transfer|misc|total)$} $k]} {
      putidx $i "syntax: $lastbind \[bot\] <irc|botnet|partyline|transfer|misc|total>"
      return
    }
  } else {
    set b [dustin_bots $dustin_control($h)]
    set k [lindex $arg 0]
  }
  if {![regexp {^(irc|botnet|partyline|transfer|misc|total)$} $k]} {
    putidx $i "syntax: $lastbind \[bot\] <irc|botnet|partyline|transfer|misc|total>"
    return
  }
  # z = total out, n = total in, u = today out, j = today in
  set z 0; set n 0; set u 0; set j 0
  foreach x $b {
    foreach e [relay $x "traffic"] {
      set y [lindex $e 0]
      if {$y == $k} {
        # in-traffic today, in-traffic total, out-traffic today, out-traffic
        set d [lindex $e 1]
        set f [lindex $e 2]
        set g [lindex $e 3]
        set h [lindex $e 4]
        set z [expr double($z + $d)]
        set n [expr double($n + $h)]
        set u [expr double($u + $f)]
        set j [expr double($j + $g)]
        putidx $i "$x: out; [stdio_size $f] ([stdio_size $d] today), in; [stdio_size $h] ([stdio_size $g] today)"
      }
    }
  }
  set l [llength $b]
  if {$l > 1} {
    putidx $i "(avg out: [stdio_size [expr double($u / $l)]] ([stdio_size [expr double($z / $l)]] today); avg in: [stdio_size [expr double($n / $l)]] ([stdio_size [expr double($j / $l)]] today))"
  }
}
proc dustin_dcc_nserv {h i arg} {
  global botnet-nick dustin_control lastbind server server-online botnick
  stdio_putcmd $h $arg
  if {![info exists dustin_control($h)]} {
    set dustin_control($h) *
  }
  if {$arg != ""} {
    if {![islinked $arg]} {
      putidx $i "syntax: $lastbind \[bot\]"
      return
    }
    putidx $i "$arg[if {[relay $arg "botnick"] != $arg} { set p " ([relay $arg "botnick"])" }]: [relay $arg "server"], [stdio_duration [expr [unixtime] - [relay $arg "server_online"]]]"
    return
  }
  set l ""; set d 0
  if {[dustin_control $dustin_control($h)]} {
    putidx $i "${botnet-nick}[if {${botnet-nick} != $botnick} { set p " ($botnick)" }]: $server, [stdio_duration [expr [unixtime] - ${server-online}]]"
    lappend l $server
    incr d [expr [unixtime] - ${server-online}]
  }
  foreach b [dustin_bots $dustin_control($h)] {
    putidx $i "$b[if {[relay $b "botnick"] != $b} { set p " ([relay $b "botnick"])" }]: [relay $b "server"], [stdio_duration [expr [unixtime] - [relay $b "server_online"]]]"
    if {[lsearch $l [relay $b "server"]] == "-1"} {
      lappend l [relay $b "server"]
    }
    incr d [expr [unixtime] - [relay $b "server_online"]]
  }
  if {[llength $l] > 1} {
    putidx $i "([llength $l] unique servers; avg connection time: [stdio_duration [expr $d / ([llength [dustin_bots $dustin_control($h)]] + 1)]])"
  }
}
proc dustin_dcc_nmsg {h i arg} {
  global botnet-nick dustin_control lastbind relay
  stdio_putcmd $h $arg
  if {![info exists dustin_control($h)]} {
    set dustin_control($h) *
  }
  set n [lindex [split $arg] 0]
  set s [join [lrange [split $arg] 1 end]]
  if {$n == "" || $s == ""} {
    putidx $i "syntax: $lastbind <nick|chan> <text>"
    return
  }
  if {[dustin_control $dustin_control($h)]} {
    putserv "PRIVMSG $n :$s"
  }
  dustin_sendcmd $dustin_control($h) "nmsg $n $s"
}
proc dustin_dcc_nact {h i arg} {
  global botnet-nick dustin_control lastbind relay
  stdio_putcmd $h $arg
  if {![info exists dustin_control($h)]} {
    set dustin_control($h) *
  }
  set n [lindex [split $arg] 0]
  set s [join [lrange [split $arg] 1 end]]
  if {$n == "" || $s == ""} {
    putidx $i "syntax: $lastbind <nick|chan> <text>"
    return
  }
  if {[dustin_control $dustin_control($h)]} {
    putserv "PRIVMSG $n :\001$s"
  }
  dustin_sendcmd $dustin_control($h) "nact $n $s"
}
proc dustin_dcc_nping {h i arg} {
  global botnet-nick dustin_control lastbind relay
  stdio_putcmd $h $arg
  if {![info exists dustin_control($h)]} {
    set dustin_control($h) *
  }
  dustin_sendcmd $dustin_control($h) "nping $i [clock clicks]"
}
proc dustin_dcc_nhash {h i arg} {
  global botnet-nick dustin_control lastbind relay
  stdio_putcmd $h $arg
  if {![info exists dustin_control($h)]} {
    set dustin_control($h) *
  }
  dustin_sendcmd $dustin_control($h) "nhash"
  if {[dustin_control $dustin_control($h)]} {
    uplevel {rehash}
  }
}
proc dustin_dcc_ndump {h i arg} {
  global botnet-nick dustin_control lastbind relay
  stdio_putcmd $h $arg
  if {![info exists dustin_control($h)]} {
    set dustin_control($h) *
  }
  if {$arg == ""} {
    putidx $i "syntax: $lastbind <text>"
    return
  }
  dustin_sendcmd $dustin_control($h) "ndump $arg"
  if {[dustin_control $dustin_control($h)]} {
    putserv $arg
  }
}
proc dustin_dcc_nsave {h i arg} {
  global botnet-nick dustin_control lastbind relay
  stdio_putcmd $h $arg
  if {![info exists dustin_control($h)]} {
    set dustin_control($h) *
  }
  dustin_sendcmd $dustin_control($h) "nsave"
  if {[dustin_control $dustin_control($h)]} {
    uplevel {save}
  }
}
proc dustin_dcc_nnotice {h i arg} {
  global botnet-nick dustin_control lastbind relay
  stdio_putcmd $h $arg
  if {![info exists dustin_control($h)]} {
    set dustin_control($h) *
  }
  set n [lindex [split $arg] 0]
  set s [join [lrange [split $arg] 1 end]]
  if {$n == "" || $s == ""} {
    putidx $i "syntax: $lastbind <nick|chan> <text>"
    return
  }
  if {[dustin_control $dustin_control($h)]} {
    putserv "NOTICE $n :$s"
  }
  dustin_sendcmd $dustin_control($h) "nnotice $n $s"
}
proc dustin_dcc_njump {h i arg} {
  global botnet-nick dustin_control lastbind relay
  stdio_putcmd $h $arg
  if {![info exists dustin_control($h)]} {
    set dustin_control($h) *
  }
  if {$arg == "" || ![regexp {^[^:]+(?:[0-9]{1,5})?} [lindex $arg 1]] || ![islinked [lindex $arg 0]]} {
    putidx $i "syntax: $lastbind <bot> \[server\] \[port\]"
    return
  }
  putbot [lindex $arg 0] "njump [lrange $arg 1 end]"
}
proc dustin_dcc_nmode {h i arg} {
  global botnet-nick dustin_control lastbind relay
  stdio_putcmd $h $arg
  if {![info exists dustin_control($h)]} {
    set dustin_control($h) *
  }
  if {$arg == "" || ![validchan [lindex $arg 0]]} {
    putidx $i "syntax: $lastbind <chan> <mode> [args]"
  }
  if {[botisop [lindex $arg 0]] && [dustin_control $dustin_control($h)]} {
    putserv "MODE [lrange $arg 0 end]"
  }
  dustin_sendcmd $dustin_control($h) "nmode $arg"
}
proc dustin_bots {i} {
  if {$i == "*"} {
    set n [bots]
  } else {
    set n $i
  } 
  set n [lsort -dictionary $n]
  return $n
}
proc dustin_control {i} {
  global botnet-nick
  if {$i == "*"} {
    return 1
  }
  if {[lsearch $i ${botnet-nick}] != "-1"} {
    return 1
  }
  return 0
}
proc dustin_smart {c x} {
  global _lag dustin_lag_limit
  if {![validchan $c] || ![regexp {^(?:deop|op|kick|devoice|voice)$} $x]} {
    return
  }
  set l ""; set d 0; set b ""
  foreach n [chanlist $c] {
    set h [nick2hand $n]
    switch -regexp -- $x {
      "deop" {
        set d "![matchattr $h of|of $c]"
      }
      "op" {
        set d "[matchattr $h o|o $c] && ![matchattr $h d|d $c]"
      }
      "kick" {
        set d "![matchattr $h of|of $c]"
      }
      "devoice" {
        set d "![matchattr $h vf|vf $c]"
      }
      "voice" {
        set d "[matchattr $h v|v $c]"
      }
    }
    if "$d && ![isbotnick $n]" {
      lappend l $n
    }
  }
  set l [stdio_randlist $l]
  foreach d [array names _lag] {
    if {$_lag($d) < $dustin_lag_limit && [onchan $d $c]} {
      lappend b $d
    }
  }
  global dustin_smart_array
  if {[array exists dustin_smart_array]} {
    unset dustin_smart_array
  }
  dustin_smart_sort $b $l
  foreach n [array names dustin_smart_array] {
    if {[isbotnick $n]} {
      dustin_smart_parse $c $x [join $dustin_smart_array($n)]
      continue
    }
    putbot $n "dustin smart $c $x [join $dustin_smart_array($n)]"
  }
  
}
proc smart_test {c} {
  global dustin_smart_array botnet-nick
  if {[array exists dustin_smart_array]} {
    unset dustin_smart_array
  }
  set b [join "${botnet-nick} [bots]"]; set l [chanlist $c]
  dustin_smart_sort [stdio_randlist $b] [stdio_randlist $l]
  foreach n [array names dustin_smart_array] {
    putlog "$n : $dustin_smart_array($n)"
  }
}
proc dustin_smart_sort {b l} {
  set b [stdio_randlist $b]; set l [stdio_randlist $l]; set x [llength $b]; set y [llength $l]
  if {$x > $y} {
    set x $y
    set b [lrange $b 0 [expr $y - 1]]
  }
  set i [expr round($y / $x)]; set e 0; set item [lindex $b $e]; set s 0; set o 0
  global dustin_smart_array
  foreach n $l {
    # append the nick into item's (a bot) array
    lappend dustin_smart_array($item) $n
    incr s; incr o
    if {$s == $i} {
      set s 0
      incr e
      set item [lindex $b $e]
      if {$item == "" && $o < $y} {
        dustin_smart_sort $b [lrange $l [expr $o - 1] end]
        break
      }
    }
  }
  return $l  
}

proc dustin_smart_parse {c x arg} {
  if {![validchan $c] || ![regexp {^(?:deop|op|kick|devoice|voice)$} $x]} {
    return
  }
  global modes-per-line _lag botnet-nick; set g ${modes-per-line}
  if {[regexp {^(?:de)?(voice|op)$} $x]} {
    switch -exact -- $x {
      "deop" {
        set m "-o"
      }
      "op" {
        set m "+o"
      }
      "devoice" {
        set m "-v"
      }
      "voice" {
        set m "+v"
      }
    }
    # "+oooo"
    set m [string index $m 0][string repeat [string index $m 1] $g]
    for {set z [llength $arg]; set y 1; set w $g} {$y <= $z} {incr y $g; incr w $g} {
      putquick "MODE $c $m [lrange $arg [expr $y - 1] [expr $w - 1]]"
    }
  } elseif {$x == "kick"} {
    # +im the channel if we're the most unlagged bot
    if {[lindex [lag_index] 0] == ${botnet-nick}} {
      putquick "MODE $c +im"
    }
    foreach n $arg {
      putquick "KICK $c $n :$n"
    }
  }
}
proc dustin_dcc_smart {h i arg} {
  global botnet-nick lastbind
  stdio_putcmd $h $arg
  if {$arg == "" || ![validchan [lindex $arg 0]] || ![regexp {^(?:deop|op|kick|devoice|voice)$} [lindex $arg 1]]} {
    putidx $i "syntax: $lastbind <chan> <deop|op|kick|devoice|voice>"
    return
  }
  dustin_smart [lindex $arg 0] [lindex $arg 1]
  putidx $i "performing smart... ([lindex $arg 1] of [lindex $arg 0])"
}
proc dustin_dcc_ntcl {h i arg} {
  global botnet-nick lastbind dustin_control
  stdio_putcmd $h $arg
  if {[stdio_is_perm_owner $h] || $arg == ""} {
    putidx $i "fuck off"; return
  }
  if {![info exists dustin_control($h)]} {
    set dustin_control($h) *
  }
  dustin_sendcmd $dustin_control($h) "ntcl $i $arg"
  if {[dustin_control $dustin_control($h)]} {
    set r [uplevel $arg]
    if {$r != ""} {
      putidx $i "tcl: $r"
    }
  }
}  
proc dustin_nkch {o n} {
  global dustin_control
  if {[info exists dustin_control($o)]} {
    set dustin_control($n) $dustin_control($o)
    unset dustin_control($o)
  }
}
proc dustin_chon {h i} {
  global uptime botnet-nick dustin_chon dustin_chon_nicklen
  putidx $i "users on the partyline:"
  set dustin_chon_nicklen [string length ${botnet-nick}]
  foreach n [whom *] {
    set dustin_chon([lindex $n 0]) $n
  }
  set l [lsort -command dustin_chon_sort [array names dustin_chon]]
  set f "%-[expr $dustin_chon_nicklen + 4].[expr $dustin_chon_nicklen + 4]s"
  if {[bots] != ""} {
    putidx $i [format "%-[expr $dustin_chon_nicklen + 6].[expr $dustin_chon_nicklen + 6]s %s"  ${botnet-nick} ${botnet-nick}]
  } else {
    putidx $i ${botnet-nick}
  }
  set n 0
  set t ""
  set m [llength $l]
  if {[set p [llength [bots]]] > $m} {
    set m $p
  }
  set p 0
  while {$m} {
    set x [lindex $l $p]
    if {$x != ""} {
      if {[lindex $l end] == $x} {
        set t "`-[format $f "[lindex $dustin_chon($x) 3]$x"]"
      } else {
        set t "|-[format $f "[lindex $dustin_chon($x) 3]$x"]"
      }
    } else {
      set t "  [format $f ""]"
    }
    if {[set b [lindex [botlist] $n]] != ""} {
      if {[lindex [botlist] end] == $b} {
        set t "$t `-[lindex $b 3][lindex $b 0]"
      } else {
        set t "$t |-[lindex $b 3][lindex $b 0]"
      }
      incr n
    } 
    incr m -1
    incr p 1
    putidx $i "  $t"
    set t ""
  }
  unset dustin_chon
  unset dustin_chon_nicklen
}
proc dustin_chon_sort {x y} {
  global dustin_chon dustin_chon_nicklen
  set c [lsearch {- @ + *} [lindex $dustin_chon($x) 3]]
  set d [lsearch {- @ + *} [lindex $dustin_chon($y) 3]]
  if {$dustin_chon_nicklen < [string length $x]} {
    set dustin_chon_nicklen [string length $x]
  }
  if {$dustin_chon_nicklen < [string length $y]} {
    set dustin_chon_nicklen [string length $y]
  }
  if {$c < $d} {
    return 1
  }
  if {$c == $d} {
    return 0
  }
  if {$c > $d} {
    return -1
  }
}
proc dustin_bot {b c arg} {
  if {$c != "dustin"} {
    return -code break
  }
  set r [lindex $arg 0]
  set e [lrange $arg 1 end]
  switch -exact -- $r {
    "njoin" {
      putlog "$b: njoin [lindex $e 0]"
      channel add [lindex $e 0]
      putserv "JOIN [join [lrange $e 0 end]]"
    }
    "npart" {
      putlog "$b: npart [lindex $e 0]"
      if {[validchan $e]} {
        channel remove $e
        putserv "PART [join [lrange $e 1 end]]"
      } 
    }
    "nmsg" {
      putlog "$b: nmsg [lindex $e 0]"
      putserv "PRIVMSG [lindex $e 0] :[join [lrange $e 1 end]]"
    }
    "nact" {
      putlog "$b: nact [lindex $e 0]"
      putserv "PRIVMSG [lindex $e 0] :\001ACTION[join [lrange $e 1 end]]"
    }
    "nnotice" {
      putlog "$b: nnotice [lindex $e 0]"
      putserv "NOTICE [lindex $e 0] :[join [lrange $e 1 end]]"
    }
    "nping" {
      putbot $b "dustin reply nping $e"
      putlog "$b: nping"
    }
    "nhash" {
      putlog "$b: nhash"
      uplevel {rehash}
    }
    "nsave" {
      putlog "$b: nsave"
      uplevel {backup; save}
    }
    "ndump" {
      putlog "$b: ndump $e"
      putserv [join $e]
    }
    "njump" {
      putlog "$b: njump $e"
      jump $e
    }
    "nmode" {
      putlog "$b: nmode [lindex $e 0]"
      putserv "MODE [lrange $e 0 end]"
    }
    "ntcl" {
      set r [uplevel [join [lrange $e 1 end]]]
      putlog "$b: ntcl"
      if {$r != ""} {
        putbot $b "reply ntcl [lindex $e 0] $r"
      }
    }           
    "reply" {
      switch -exact -- [lindex $e 0] {
        "nping" {
          putidx [lindex $e 1] "$b: [lag_round [expr double([clock clicks] - [lindex $e 2]) / 10000000] "3"]s"
        }
        "ntcl" {
          putidx [lindex $e 1] "$b: rntcl [lrange $e 2 end]"
        }
      }
    }
    "smart" {
      putlog "$b: smart [lindex $e 1] [lindex $e 0]"
      dustin_smart_parse [lindex $e 0] [lindex $e 1] [lrange $e 2 end]
    }
    "op_wantops" {
      set n [lindex $e 1]; set c [lindex $e 0]
      if {[validchan $c]} {
        if {[botisop $c] && [onchan $n $c] && [matchattr $b o|o $c] && ![matchattr $b d|d $c]} {
          stdio_cookie_op $n $c
        }
      }
    }
    "op_wantkey" {
      set c [lindex $e 0]
      if {[validchan $c] && [string match "*k*" [lindex [split [getchanmode $c]] 0]]} {
        putbot $b "dustin op_chankey $c [lindex [split [getchanmode $c]] 1]"
      }
    }
    "op_chankey" {
      putserv "JOIN $e"
    }
    "op_wantinvite" {
      set c [lindex $e 0]; set n [lindex $e 1]
      if {[validchan $c]} {
        if {![onchan $n $c] && [botisop $c]} {
          putserv "INVITE $n $c"
        }
      }
    }
    "op_wantunban" {
      set c [lindex $e 0]; set m [lindex $e 1]; set n [lindex $e 2]; set i 0
      if {[validchan $c]} {
        foreach x [chanbans $c] {
          set j [lindex $x 0]; set k [lindex [split [lindex $x 1] !] 0]
          if {![matchattr [nick2hand $k] mn|mn $c] && [string match $j $m]} {
            pushmode $c -b $j
            incr i
          }
        }
        if {$i} {
          flushmode $c
          putserv "INVITE $n $c"
        }
      }
    }
    "op_wantlimit" {
      set c [lindex $e 0]; set n [lindex $e 1]
      if {[validchan $c] && [onchan $n $c]} {
        putquick "MODE $c +l [expr [llength [chanlist $c]] + 10]"
        putquick "INVITE $n $c"
      }
    }
  }
}
proc dustin_sendcmd {b arg} {
  if {$b == "*"} {
    putallbots "dustin $arg"
  } else {
    foreach x [split $b ,] {
      putbot $b "dustin $arg"
    }
  }
}
bind bot -|- dustin dustin_bot
bind mode -|- "* +o" dustin_check_ops
bind need -|- "*" dustin_needed
bind dcc n|- njoin dustin_dcc_njoin
bind dcc n|- npart dustin_dcc_npart
bind dcc n|- nshell dustin_dcc_nshell
bind dcc n|- ntraffic dustin_dcc_ntraffic
bind dcc n|- nmsg dustin_dcc_nmsg
bind dcc n|- nact dustin_dcc_nact
bind dcc n|- nping dustin_dcc_nping
bind dcc n|- nserv dustin_dcc_nserv
bind dcc n|- ncontrol dustin_dcc_ncontrol
bind dcc n|- nhash dustin_dcc_nhash
bind dcc n|- ndump dustin_dcc_ndump
bind dcc n|- nsave dustin_dcc_nsave
bind dcc n|- nnotice dustin_dcc_nnotice
bind dcc n|- njump dustin_dcc_njump
bind dcc n|- nmass dustin_dcc_nmass
bind dcc n|- smart dustin_dcc_smart
bind dcc n|- ntcl dustin_dcc_ntcl
bind nkch n|- "*" dustin_nkch
bind chon -|- "*" dustin_chon


