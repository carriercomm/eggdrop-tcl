#
# relay.tcl - facillates the periodic broadcast of certain bot-specific info
#             to all other bots. (e.g. this bot's uptime/hostname/version)
#

# [0-9]+ delay for relaying? (60s should be fine)
set relay_delay 60
set relay_elements "server server_online botnick version uptime unames traffic hostname config tclversion modules"
proc relay {b i} {
  global relay
  if {[islinked $b] && [info exists relay($b,$i)]} {
    return $relay($b,$i)
  }
  return -code continue
}
proc relay_broadcast {} {
  global botnet-nick relay_delay
  set m ${botnet-nick}; set h ""
  foreach b [botlist] {
    set a([lindex $b 0]) [lindex $b 1]
  }  
  foreach b [array names a] {
    if {$a($b) != $m && [lsearch $h $a($b)] == "-1"} {
      lappend $h $a($b)
    } elseif {$a($b) == $m} {
      relay_putbot $b $m
    }
  }
  foreach s $h {
    relay_putbot $s $m
  }
  utimer $relay_delay relay_broadcast 
}
proc relay_putbot {s m} {
  global server config botnick version uptime server-online
  catch {set server} x
  if {$x != ""} {
    putbot $s "relay $m server $x"
    putbot $s "relay $m server_online ${server-online}"
  }
  catch {set botnick} x
  if {$x != ""} {
    putbot $s "relay $m botnick $x"
  }
  putbot $s "relay $m version $version"
  putbot $s "relay $m uptime $uptime"
  putbot $s "relay $m unames [unames]"
  putbot $s "relay $m traffic [traffic]"
  putbot $s "relay $m hostname [info hostname]"
  putbot $s "relay $m config $config"
  putbot $s "relay $m tclversion [info tclversion]"
  #putbot $s "relay $m modules [modules]"
  catch {exec uptime} u
  regsub -all {\s{2}} $u {\s} u
  putbot $s "relay $m uptime [lrange $u 1 end]"
  
}
proc relay_kill_utimer {c} {
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
proc relay_bot {b c arg} {
  global relay relay_elements botnet-nick relay_unixtime
  set m ${botnet-nick}; set o [lindex $arg 0]
  catch {set relay($o,[lindex $arg 1]) [lrange $arg 2 end]} n
  if {[lindex $arg 1] == "uptime"} {
    foreach e $relay_elements {
      if {![info exists relay($o,$e)]} {
        set relay($o,$e) ""
      }
    }
    set relay_unixtime($o) [unixtime]
  }
  foreach z [botlist] {
    set h [lindex $z 1]
    if {$h == $m && [lindex $z 0] != $b} {
       putbot [lindex $z 0] "relay $arg"
    }
  }  
}
bind bot - relay relay_bot
relay_kill_utimer relay_broadcast
relay_broadcast