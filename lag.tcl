#
# lag.tcl - has each bot broadcast its current server lag to all other bots.
#           also provides a partyline command for checking the lag of all bots.
#

# [int] delay for lag checking? (20s == default)
set lag_delay 20
proc lag_bot {b c arg} {
  if {$c != "lag"} {
    return
  }
  global _lag botnet-nick lag_unixtime
  set x [lindex $arg 0]
  set n [lindex $arg 1]
  set _lag($x) $n
  set lag_unixtime($x) [unixtime]
  set m ${botnet-nick}
  foreach z [botlist] {
    set h [lindex $z 1]
    if {$h == $m && [lindex $z 0] != $b} {
       putbot [lindex $z 0] "lag $x $n"
    }
  }
  foreach z [array names _lag] {
    if {![islinked $z]} {
      unset _lag($z)
    }
  }
}
proc lag_dcc {h i arg} {
  stdio_putcmd $h $arg
  global lag_lastsent
  set lag_lastsent [clock clicks]
  putserv "lag_checklag_${lag_lastsent}_info_$i"
  putidx $i "checking lag..."
}
proc lag_dcc_nlag {h i arg} {
  stdio_putcmd $h $arg
  putidx $i "listing botlag..."
  global _lag lag_unixtime
  set x ""; set n 0
  foreach z [array names _lag] {
    lappend x $z
  }
  set x [lsort -command lag_sort $x]
  foreach l $x {
    putidx $i "$l: [lag_round $_lag($l) 3]s (last update: [strftime "%H:%M" $lag_unixtime($l)])"
    set n [expr $n + $_lag($l)]
  }
  set n [lag_round [expr double($n) / double([llength $x])] 3]
  putidx $i "([llength $x] bot[if {[llength $x] != 1} { set p "s" }] listed; avg lag: ${n}s)"
}
proc lag_sort {i n} {
  global _lag
  if {$_lag($i) < $_lag($n)} {
    return -1
  }
  if {$_lag($i) == $_lag($n)} {
    return 0
  }
  if {$_lag($i) > $_lag($n)} {
    return 1
  }
}
proc lag_nkch {o n} {
  if {$o == $n} {
    return
  }
  global _lag lag_unixtime
  if {[info exists _lag($o)]} {
    set _lag($n) $_lag($o)
    set lag_unixtime($n) $lag_unixtime($o)
    unset _lag($o); unset lag_unixtime($o)
  }
}

proc lag_index {} {
  global _lag
  lsort -command lag_sort [array names _lag]
}
proc lag_checklag {} {
  global lag _lag lag_lastsent lag_delay lag_unixtime
  if {![info exists lag]} {
    set lag 0
  }
  if {![array exists _lag]} {
    foreach b [bots] {
      set _lag($b) 0
    }
  }
  if {![array exists lag_unixtime]} {
    foreach b [bots] {
      set lag_unixtime($b) [unixtime]
    }
  }
  set lag_lastsent [clock clicks]
  putserv "lag_checklag_$lag_lastsent"
  utimer $lag_delay lag_checklag
}
proc lag_updatelag {f k t} {
  set c [lindex [split $t] 1]
  if {![regexp {^lag_checklag_[-\d]+} $c]} {
    return
  }
  set n [expr abs([lindex [split $c _] 2])]; set x [expr abs([clock clicks])]
  if {$n < 0} {
    return
  }
  global lag _lag botnet-nick lag_unixtime
  set lag [expr abs(double($n - $x) / 1000000)]
  set _lag(${botnet-nick}) $lag
  set lag_unixtime(${botnet-nick}) [unixtime]
  if {[regexp {^lag_checklag_[-\d]+_info_\d+$} $c]} {
    set i [lindex [split $c _] 4]
    if {![valididx $i]} {
      return
    }
    set f [lag_round $lag 3]
    putidx $i "current lag: ${f}s"
    return
  }
  lag_broadcast
}
proc lag_broadcast {} {
  global lag botnet-nick
  set m ${botnet-nick}; set h ""
  foreach b [botlist] {
    set a([lindex $b 0]) [lindex $b 1]
  }  
  foreach b [array names a] {
    if {$a($b) != $m && [lsearch $h $a($b)] == "-1"} {
      lappend $h $a($b)
    } elseif {$a($b) == $m} {
      putbot $b "lag $m $lag"
    }
  }
  foreach s $h {
    putbot $s "lag $m $lag"
  }
}
proc lag_putdot {i} {
  return [string range $i 0 0].[string range $i 1 end]
}
proc lag_round {n i} {
  set n [expr abs($n)]
  set i [expr abs($i)]
  if {![regexp {^\d+.\d+$} $n]} {
    return $n
  }
  set n [split $n .]
  set f [lindex $n 0]; set r [string range [lindex $n 1] 0 [expr $i - 1]]
  set n $f.$r
  return $n
}
proc lag_kill_utimer {c} {
  set n 0
  regsub -all -- {([\[\]])} $c {\\\1} c
  foreach t [utimers] {
    if {[string match $c [join [lindex $t 1]]]} {
      killutimer [lindex $t 2]
      incr n
    }
  }
  return $n
}
bind raw -|- 421 lag_updatelag
bind dcc mnt|- lag lag_dcc
bind dcc n|- nlag lag_dcc_nlag
bind bot -|- lag lag_bot
bind nkch b|- "*" lag_nkch
lag_kill_utimer lag_checklag
lag_checklag

