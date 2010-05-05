#
# stdio.tcl - provides several "standard" procedures for other scripts to use
#             as well as many partyline commands.
#             (there are several silly features in here. I was 14 when I wrote this.)
#


# [0/1] use cookie ops?
set stdio_use_cookie_op 0
# [0/1] turn on "owner prot?"
set stdio_use_owner_prot 0
if {$stdio_use_owner_prot} {
  # [0/1] kick offending nicks?
  set stdio_owner_prot(do_kick) 0
  # [0/1] deop offending nicks?
  set stdio_owner_prot(do_deop) 1
  # [0/1] ban offending nicks?
  set stdio_owner_prot(do_ban) 1
  # [0/1] +k offending nicks?
  set stdio_owner_prot(do_k) 0
  # [0/1] +d offending nicks?
  set stdio_owner_prot(do_d) 1
}   
proc stdio_is_set {v} {
  return [info exists $v]
}
proc stdio_is_mask {m} {
  return [regexp {^([^!@ ]+![^!@ ]+@[^!@ ]+)$} $m]
}
proc stdio_is_chan {c} {
  return [regexp {^[!#&\+][^ ,]+$} $c]
}
proc stdio_is_irc_nick {n} {
  return [regexp {^[^ ,]+$} $n]
}
proc stdio_is_valid_filename {f} {
  return [regexp {^([^\/\\:\*\?"\<\>\|])+$} $f]
}
proc stdio_is_perm_owner {h} {
  global owner
  regsub -all -- , [string tolower $owner] "" o
  if {[matchattr $h n] && [lsearch -exact $o [string tolower $h]] != -1} {
    return 1
  }
  return 0
}
proc stdio_match_botattr {b f} {
  foreach f [split $f ""] {
    if {[lsearch -exact [split [botattr $b] ""] $f] == -1} {
      return 0
    }
  }
  return 1
}
proc stdio_putcmd {h arg} {
  global lastbind
  putlog "#$h# $lastbind $arg"
}
proc stdio_size {n} { 
  set b [list "B" "KB" "MB" "GB"]; set i 0; set n [expr $n + 0.0] 
  while {$n >= 1024} { 
    incr i 
    set n [expr $n / 1024] 
  } 
  if {[regexp {^\d+\.\d$} $n]} {
    set n [lindex [split $n .] 0].[string range [lindex [split $n .] 1] 0 1]0
    set n [string range $n 0 4]
  }
  regsub -- {\.(\d{2})\d+$} $n {.\1} n
  return $n[lindex $b $i]
}
proc stdio_duration {t} {
  set t [duration $t]
  regsub -all {(\d+) ([a-z])[a-z]+( |$)} $t {\1\2} t
  return $t
}
proc stdio_avg_hops {} {
  global botnet-nick stdio_avg_hops
  set i 0; set b [botlist]; set s ${botnet-nick}; set n 0
  foreach a $b {
    set stdio_avg_hops([lindex $a 0]) [lindex $a 1]
  }
  foreach a [array names stdio_avg_hops] {
    incr i [stdio_hops $a ${botnet-nick}]
  }
  set n [expr double($i) / double([llength $b])] 
  if {[regexp {^\d+\.\d+$} $n]} {
    set n [lindex [split $n .] 0].[expr round([stdio_put_dot [lindex [split $n .] 1]])]
  }
  unset stdio_avg_hops
  return $n
}
proc stdio_hops {l s} {
  global stdio_avg_hops
  set c 1
  if {[set h $stdio_avg_hops($l)] != $s} {
    incr c [stdio_hops $h $s]
  }
  return $c
}
proc stdio_put_dot {n} {
  return [string range $n 0 0].[string range $n 1 end]
}
proc stdio_dcc_action {i t} {
  return ".me [string trim [join [lrange [split $t] 1 end]] \001]"
}
proc stdio_dcc_action_ctcp {i t} {
  return ".me [string trim [join [lrange [split $t] 2 end]] \001]"
}
proc stdio_telnet_action {i t} {
  return ".me [join [lrange [split $t] 1 end]]"
}
proc stdio_dcc_wget {h i arg} {
  stdio_putcmd $h $arg
  set u [lindex $arg 0]
  if {$u == ""} {
    putidx $i "syntax: .wget <url>"; return 0
  }
  if {![stdio_is_perm_owner $h]} {
    putidx $i "you must be perm owner to use this"; return 0
  }
  catch {exec wget $u}
  putidx $i "ok, executed wget command on shell"
}
proc stdio_dcc_dns {h i arg} {
  stdio_putcmd $h $arg
  if {[set l [lindex $arg 0]] == ""} {
    putidx $i "syntax: .dns <host or ip>"; return
  }
  putidx $i "looking up $l..."
  set l [split $l]
  dnslookup $l stdio_dcc_dns_resolve_callback $i $l
}
proc stdio_dcc_dns_resolve_callback {r h s i l} {
  if {![valididx $i]} {
    return 0
  } elseif {!$s} {
    putidx $i "unable to resolve $l"
  } elseif {[regexp -nocase -- $r $l]} {
    putidx $i "resolved $r to $h"
  } else {
    putidx $i "resolved $h to $r"
  }
}
proc stdio_dcc_botnick {h i arg} {
  global altnick nick
  stdio_putcmd $h $arg
  set n [lindex [split $arg] 0]
  if {$n == ""} {
    putidx $i "syntax: botnick <newnick>"
    return 0
  }
  if {$n == "-altnick"} {
    set n $altnick
  }
  while {[regsub -- \\? $n [rand 10] n]} {
    continue
  }
  putidx $i "changing nick to '$n'"
  set nick $n
}
proc stdio_cookie_op {n c} {
  global stdio_use_cookie_op botnick
  if {![validchan $c]} {
    return
  }
  if {[botisop $c] && ![isop $n $c] && [onchan $n $c]} {
    if {!$stdio_use_cookie_op} {
      putquick "MODE $c +o $n"
    } else {
      putquick "MODE $c +o-b $n *!*@[md5 "$botnick->$n@$c:[clock clicks][rand 10000][unixtime]"]"
    }
  }
}
proc stdio_dcc_op {h i arg} {
  stdio_putcmd $h $arg
  set n [lindex [split $arg] 0]; set c [lindex [split $arg] 1]; set b 0
  if {$n == ""} {
    putidx $i "syntax: op <nick> \[channel\]"; return 0
  }
  if {$c != ""} {
    if {![validchan $c]} {
      putidx $i "no such channel '$c'"; return 0
    }
    if {![onchan $n $c]} {
      putidx $i "$n is not on $c"; return 0
    }
    if {[isop $n $c]} {
      putidx $i "$n is already opped on $c"; return 0
    }
    if {[matchattr $h o|o $c] && ![matchattr $h d|d $c] && ([string tolower [nick2hand $n $c]] == [string tolower $h] || [matchattr $h nm])} {
      if {[botisop $c]} { 
        stdio_cookie_op $n $c
      } else {
        set b 1
        foreach x [stdio_randlist [chanlist $c]] {
          set m [nick2hand $x]
          if {[matchattr $m b] && [islinked $m] && [isop $x $c] && $b <= 3} {
            putbot $m "stdio dcc_op $n $c"
            incr b
          }
        }
      }
    }
  } else {
    set l ""; set b 0
    foreach y [stdio_randlist [channels]] {
      if {[botisop $y]} {
        if {[matchattr $h o|o $y] && ![matchattr $h d|d $y] && ![isop $n $y] && [onchan $n $y] && ([string tolower [nick2hand $n $y]] == [string tolower $h] || [matchattr $h nm])} {
          stdio_cookie_op $n $y
          lappend l $y
        }
      } else {
        set b 1
        foreach x [stdio_randlist [chanlist $y]] {
          set z [nick2hand $x]
          if {[matchattr $z b] && [islinked $z] && [isop $x $y] && $b <= 3} {
            putbot $z "stdio dcc_op $n $y"
            incr b
          }
        }
      }
    }
  }
}
proc stdio_msg_op {o u h arg} {
  if {$h == "*"} {
    return
  }
  set p [lindex [split $arg] 0]
  set c [lindex [split $arg] 1]
  if {[passwdok $h $p] == 0} {
    return
  }
  if {$c == ""} {
    foreach x [channels] {
      if {[botonchan $x] && [botisop $x] && [onchan $o $x] && ![isop $o $x] && [matchattr $h o|o $x] && ![matchattr $h d|d $x]} {
        stdio_cookie_op $o $x
      }
    }
    return
  } else {
    if {[matchattr $h o|o $c] && ![matchattr $h d|d $c]} {
      stdio_cookie_op $o $c
    }
  }
  return
}
proc stdio_dcc_mode {h i arg} {
  stdio_putcmd $h $arg
  set c [lindex $arg 0]; set p [lrange $arg 1 end]
  if {![validchan $c] || ![matchattr $h o|o $c] || $p == ""} {
    putidx $i "error."
    return
  }
  putserv "MODE $c $p"
  putidx $i "changed mode on $c."
}
proc stdio_dcc_notice {h i arg} {
  putlog "#$h# notice $arg"
  set n [lindex $arg 0]; set t [lrange $arg 1 end]
  if {![matchattr $h m] || $t == ""} {
    putidx $i "error."
  }
  putserv "NOTICE $n :$t"
  putidx $i "notice to $n: $t"
}
proc stdio_dcc_ctcp {h i arg} {
  putlog "#$h# ctcp $arg"
  set n [lindex $arg 0]; set r [string toupper [lindex $arg 1]]; set t [lrange $arg 2 end]
  if {![matchattr $h m] || $r == "" || [regexp {^(DCC|ACTION)$} $r]} {
    putidx $i "error."
  }
  if {$r == "PING"} {
    set t [unixtime]
  }
  putserv "PRIVMSG $n :\001$r $t"
  putidx $i "ctcp to $n: $r"
}
proc stdio_dcc_althub {h i arg} {
  putlog "#$h# althub $arg"
  global botnet-nick
  set z [lindex $arg 0]; set x [lindex $arg 1]
  if {$z == ""} {
    set k ""
    foreach b [bots] {
      if {[stdio_match_botattr $b a] && [islinked $b]} {
        lappend k $b
      }
    }
    if {$k != ""} {
      putidx $i "current althubs: [join $k ", "]."
    } else {
      putidx $i "no althubs are currently linked."
    }
    return
  } 
  if {$x == ""} {
    if {![matchattr $z b]} {
      putidx $i "$z isn't a bot."; return
    }
    if {$z == ${botnet-nick}} {
      putidx $i "i can't make myself an althub..."; return
    }
    botattr $z +ap
    putidx $i "changed $z's bot flags to [botattr $z]"
    return
  }
  if {![matchattr $z b]} {
    putidx $i "$z isn't a bot."; return
  }
  if {![matchattr $x b]} {
    putidx $i "$x isn't a bot."; return
  }
  if {![islinked $z] || (![islinked $x] && $x != ${botnet-nick})} {
    putidx $i "both bots must be linked."; return
  }
  putbot $z "stdio dcc_althub $h $x"
  putidx $i "sent althub change to $z."
}
proc stdio_dcc_hub {h i arg} {
  putlog "#$h# hub $arg"
  global botnet-nick
  set z [lindex $arg 0]; set x [lindex $arg 1]
  if {$z == ""} {
    set k ""
    foreach b [bots] {
      if {[stdio_match_botattr $b h] && [islinked $b]} {
        lappend k $b
      }
    }
    if {$k != ""} {
      putidx $i "current hubs: [join $k ", "]."
    } else {
      putidx $i "no hubs are currently linked."
    }
    return
  } 
  if {$x == ""} {
    if {![matchattr $z b]} {
      putidx $i "$z isn't a bot."; return
    }
    if {$z == ${botnet-nick}} {
      putidx $i "i can't make myself a hub..."; return
    }
    botattr $z +hgp
    putidx $i "changed $z's bot flags to [botattr $z]"
    return
  }
  if {![matchattr $z b]} {
    putidx $i "$z isn't a bot."; return
  }
  if {![matchattr $x b]} {
    putidx $i "$x isn't a bot."; return
  }
  if {![islinked $z] || (![islinked $x] && $x != ${botnet-nick})} {
    putidx $i "both bots must be linked."; return
  }
  putbot $z "stdio dcc_hub $h $x"
  putidx $i "sent hub change to $z."
}
proc stdio_dcc_leaf {h i arg} {
  putlog "#$h# leaf $arg"
  global botnet-nick
  set z [lindex $arg 0]; set x [lindex $arg 1]
  if {$z == ""} {
    set k ""
    foreach b [bots] {
      if {[stdio_match_botattr $b gs] && [islinked $b]} {
        lappend k $b
      }
    }
    if {$k != ""} {
      putidx $i "current leafs: [join $k ", "]."
    } else {
      putidx $i "no leafs are currently linked."
    }
    return
  } 
  if {$x == ""} {
    if {![matchattr $z b]} {
      putidx $i "$z isn't a bot."; return
    }
    if {$z == ${botnet-nick}} {
      putidx $i "i can't make myself a leaf..."; return
    }
    botattr $z +gs
    putidx $i "changed $z's bot flags to [botattr $z]"
    return
  }
  if {![matchattr $z b]} {
    putidx $i "$z isn't a bot."; return
  }
  if {![matchattr $x b]} {
    putidx $i "$x isn't a bot."; return
  }
  if {![islinked $z] || (![islinked $x] && $x != ${botnet-nick})} {
    putidx $i "both bots must be linked."; return
  }
  putbot $z "stdio dcc_leaf $h $x"
  putidx $i "sent leaf change to $z."
}
proc stdio_dcc_bots {h i arg} {
  putlog "#$h# bots $arg"
  global botnet-nick
  set b [userlist +b]; set l [bots]; set u ""
  foreach x $b {
    if {![islinked $x] && $x != ${botnet-nick}} {
      lappend u $x
    }
  }
  if {$l == ""} {
    putidx $i "no bots linked."
  } else {
    putidx $i "bots: [join $l ", "]"
    if {$u != ""} {
      putidx $i "unlinked: [join $u ", "]"
    }
    set x [expr [llength $l] + 1]; set y [llength $b]
    putidx $i "x: $x, y: $y"
    putidx $i "(total: $x, [expr round((double($x) / double($y)) * 100)]%; avg hops: [stdio_avg_hops])"
  }
       
}
proc stdio_randlist {l} {
  set r ""
  while {[llength $l] > 0} {
    set n [rand [llength $l]]
    lappend r [lindex $l $n]
    set l [lreplace $l $n $n]
  }
  return $r
}
proc stdio_do_owner_prot {n u h c t r} {
  if {[matchattr $h nftmb|nftm $c] || ![botisop $c] || ![matchattr [nick2hand $t] n]} {
    return
  }
  global stdio_owner_prot
  set i 0
  if {$stdio_owner_prot(do_deop) && [isop $n $c]} {
    pushmode $c -o $n
    incr i
  }
  if {$stdio_owner_prot(do_ban)} {
    pushmode $c +b "*!*@[lindex [split $u @] 1]"
    incr i
  }
  if {$stdio_owner_prot(do_kick)} {
    flushmode $c
    putserv "KICK $c $n :$n"
    incr i
  }
  if {$h != "*"} {
    if {$stdio_owner_prot(do_d)} {
      chattr $h -|d $c
      incr i
    }
    if {$stdio_owner_prot(do_k)} {
      chattr $h -|k $c
      incr i
    }
  }
  if {$i} {
    putlog "(owner prot:$c) $n ($u) kicked $t"
  }
}
proc stdio_dcc_cookie {h i arg} {
  global lastbind
  stdio_putcmd $h $arg
  if {[regexp {^(?:off|on|[01])$} $arg]} {
    set n [string tolower $arg]
    switch -exact $arg {
      "0" {
        set n off
      }
      "1" {
        set n on
      }
    }
    global stdio_use_cookie_op
    set stdio_use_cookie_op [regexp {on} $n]
    putidx $i "cookie ops have been turned $n..."
    putallbots "stdio cookie $n"
  } else {
    putidx $i "syntax: $lastbind <on|off>"
  }
}
proc stdio_dcc_userlist {h i arg} {
  global lastbind
  stdio_putcmd $h $arg
  if {$arg != ""} {
    set l [userlist $arg]
  } else {
    set l [userlist]
  }
  putidx $i "userlist: $l"
}
proc stdio_dcc_invite {h i arg} {
  global lastbind
  stdio_putcmd $h $arg
  if {$arg == ""} {
    putidx $i "syntax: $lastbind <nick> \[channel\]"
    return
  }
  if {[llength $arg] == 1} {
    foreach x [channels] {
      if {([matchattr [nick2hand $arg] o|o $x] || [matchattr $h m|m $x]) && [regexp i [lindex [getchanmode $x] 0]]} {
        putserv "INVITE [lindex $arg 0] $x"
      }
    }
  } else {
    if {[validchan [lindex $arg 1]] && ([matchattr [nick2hand [lindex $arg 0]] o|o $x] || [matchattr $h m|m $x])} {
      putserv "INVITE [lrange $arg 0 1]"
    } else {
      putidx $i "no such channel"
      return
    }
  }
}
proc stdio_dcc_send {h i arg} {
  global lastbind
  stdio_putcmd $h $arg
  if {$arg == ""} {
    putidx $i "syntax: $lastbind <nick> <file>"
    return
  }
  set n [lindex $arg 0]; set f [lrange $arg 1 end]
  switch -exact -- [dccsend $f $n] {
    "0" {
      putidx $i "sending $f to $n"
    }
    "1" {
      putidx $i "can't send $f (too many connections)"
    }
    "2" {
      putidx $i "can't send $f (can't open socket)"
    }
    "3" {
      putidx $i "$f does not exist"
    }
    "4" {
      putidx $i "can't send $f ($nick has too many transfers)"
    }
  }
}
proc stdio_cmd_from_bot {b c arg} {
  set c [lindex [split $arg] 0]
  switch -exact -- $c {
    "dcc_op" {
      set n [lindex [split $arg] 1]; set y [lindex [split $arg] 2]; set h [nick2hand $n $y]
      if {![validchan $y]} {
        return
      }
      if {![onchan $n $y] || [isop $n $y]} {
        return
      }
      if {[matchattr $h o|o $y] && ![matchattr $h d|d $y] && [botisop $y] && (([matchattr $h o|o $y] && ![matchattr $h d|d $y]) || [matchattr $h m|m $y])} {
        stdio_cookie_op $n $y
      }
    }
    "dcc_hub" {
      set h [lindex [split $arg] 1]; set x [lindex [split $arg] 2]
      if {[matchattr $x b]} {
        botattr $x -as+ghp
        putbot $b "stdio dcc_hubr $h changed $x's bot flags to [botattr $x]"
        putlog "$b: hub $x"
      }
    }
    "dcc_hubr" {
      set h [lindex [split $arg] 1]; set x [lrange [split $arg] 2 end]
      putlog "$b: $x"
    }
    "dcc_althub" {
      set h [lindex [split $arg] 1]; set x [lindex [split $arg] 2]
      if {[matchattr $x b]} {
        botattr $x -hg+ap
        putbot $b "stdio dcc_hubr $h changed $x's bot flags to [botattr $x]"
        putlog "$b: althub $x"
      }
    }
    "dcc_leaf" {
      set h [lindex [split $arg] 1]; set x [lindex [split $arg] 2]
      if {[matchattr $x b]} {
        botattr $x -hpa+gs
        putbot $b "stdio dcc_hubr $h changed $x's bot flags to [botattr $x]"
        putlog "$b: leaf $x"
      }
    }
    "cookie" {
      putlog "$b: cookie $e"
      global stdio_use_cookie_op
      set stdio_use_cookie_op [regexp {^on$} $e]
    }
  }
}


bind filt -|- "/me *" stdio_telnet_action
bind filt -|- "\001ACTION *\001" stdio_dcc_action
bind filt -|- "CTCP_MESSAGE \001ACTION *\001" stdio_dcc_action_ctcp
bind dcc n|- wget stdio_dcc_wget
bind dcc -|- dns stdio_dcc_dns
bind dcc m|- botnick stdio_dcc_botnick
bind bot -|- stdio stdio_cmd_from_bot
bind dcc o|o op stdio_dcc_op
bind dcc o|o mode stdio_dcc_mode
bind dcc m|- notice stdio_dcc_notice
bind dcc m|- ctcp stdio_dcc_ctcp
bind dcc n|- hub stdio_dcc_hub
bind dcc n|- althub stdio_dcc_althub
bind dcc n|- leaf stdio_dcc_leaf
bind dcc n|- cookie stdio_dcc_cookie
bind dcc -|- bots stdio_dcc_bots
bind dcc -|- userlist stdio_dcc_userlist
bind dcc o|o invite stdio_dcc_invite
bind dcc n|- send stdio_dcc_send
catch { unbind msg -|- op *msg:op }
bind msg -|- op stdio_msg_op
if {$stdio_use_owner_prot} {
  bind kick -|- "*" stdio_do_owner_prot
} else {
  catch { unbind kick -|- "*" stdio_do_owner_prot }
}
  