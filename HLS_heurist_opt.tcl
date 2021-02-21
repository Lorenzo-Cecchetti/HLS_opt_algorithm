#FUNCTION DECLARATIONS

#recursive function used in the second algorithm to update the subgraph rooted at a certain node
proc sub_graph_change {i orig_i schedule delay_diff new_fu} {
	set node_start_time [lindex [lindex $schedule $i] 1]
	set node_slack [lindex [lindex $schedule $i] 2]
	set new_start_time $node_start_time
	set new_slack $node_slack
	#if the node is not the root of the subgraph
	if {$i != $orig_i} {
		#if the new fu is faster than the current one, reduce the start time and increment the slack of the node's children
		if {$delay_diff < 0} {
			set new_start_time [expr {$node_start_time + $delay_diff}]
			set new_slack [expr {$node_slack + $delay_diff}]
			set node_new_attr "[lindex [lindex $schedule $i] 0] $new_start_time $new_slack [lindex $schedule $i 3]"
		} else {
			#if the new fu is slower, update the start time and the slack only if the start time is >= of the parent end time
			set node [lindex $schedule $i 0]
			set parents [get_attribute $node parents]
			foreach parent_node $parents {
				set idx [lsearch -index 0 $schedule $parent_node]
				set parent_end [expr {[lindex $schedule $idx 1] + [get_attribute [lindex $schedule $idx 3] delay] -1}]
				set end_start_diff [expr {$parent_end - $new_start_time}]
				if {$end_start_diff >= 0} {
					set new_start_time [expr {$new_start_time + $end_start_diff +1}]
					set new_slack [expr {$new_slack - ($end_start_diff +1)}]
				} 
			}
			set node_new_attr "[lindex [lindex $schedule $i] 0] $new_start_time $new_slack [lindex $schedule $i 3]"
		}
	} else {
		#if the node is the root of the subgraph, update only the slack
		set new_slack [expr {$node_slack - $delay_diff}]
		set node_new_attr "[lindex $schedule $i 0] [lindex $schedule $i 1] $new_slack $new_fu"
		if {$new_slack < 0} {
			return 0
		}
	}
	#modify the schedule and call the function for each child of the current node
	set new_schedule [lreplace $schedule $i $i $node_new_attr] 
	foreach child [get_attribute [lindex [lindex $new_schedule $i] 0] children] {
		set j [lsearch -index 0 $schedule $child]
		set new_schedule [sub_graph_change $j $orig_i $new_schedule $delay_diff $new_fu]
		if {$new_schedule == 0} {
			return 0
		} 
	}
	if {$schedule == 0} {
		return 0
	} else {
		return $new_schedule
	}
}

#*********************************Procedure brave_opt************************************************
proc brave_opt args {
  array set options {-lambda 0}
  if { [llength $args] != 2 } {
    return -code error "Use brave_opt with -lambda \$latency_value\$"
  }
  foreach {opt val} $args {
    if {![info exist options($opt)]} {
      return -code error "unknown option \"$opt\""
    }
    set options($opt) $val
  }
  set latency_value $options(-lambda)

  puts $latency_value

  set lambda $latency_value
 #*************************LIBRARY MANAGER **********************************************************
	set library_list [get_lib_fus]

	set fast_fu_list [list]
	set alternative_fu_list [list]

	#the next two cycles are used for the creation of the library containing the fastest fu for each type and the library containing all the remaining ones
	foreach fu $library_list {
		set isAppend 0
		foreach fast_fu $fast_fu_list {
			#Check duplicates.
			if {[get_attribute $fu operation] == [get_attribute $fast_fu operation]} {
				set isAppend 1
			}
		}
		if {$isAppend == 0} {
			lappend fast_fu_list $fu
		} else {
			lappend alternative_fu_list $fu
		}
	}
	foreach alt_fu $alternative_fu_list {
		foreach fast_fu $fast_fu_list {
			#Find the duplicate.
			if {[get_attribute $alt_fu operation] == [get_attribute $fast_fu operation]} {
				#Fixed alt_fu and fast_fu same operation.
				#Check if the alternative is faster and swap is necessary.
				if {[get_attribute $alt_fu delay] < [get_attribute $fast_fu delay]} {
					#Swap.
					set tmp $fast_fu
					set f_index [lsearch $fast_fu_list $fast_fu]
					set a_index [lsearch $alternative_fu_list $alt_fu]
					lset fast_fu_list $f_index $alt_fu
					lset alternative_fu_list $a_index $tmp
				}
			}
		}
	}

	#the next two cycles are used to create the hash_alternative_fu_list that is organized in the following way:
	# hash_alternative_fu_list : {{operation} {fu_list}}
	#fu_list : {fu_name performance_difference}
	# where performance difference is the difference between the product of power and delay of the correspondent fast_fu and the product of power and delay of the current fu 

	set hash_alternative_fu_list [list]
	set node_fu {{} {}}
	foreach alt_fu $alternative_fu_list {
		set isAppend 0
		foreach hash_alt_fu $hash_alternative_fu_list {
			#If component is not in list.
			if {[get_attribute $alt_fu operation] == [lindex $hash_alt_fu 0]} {
				set isAppend 1
			}
		}
		set operation [get_attribute $alt_fu operation]
		if {$isAppend == 0} {
			lset node_fu 0 $operation
			lset node_fu 1 $alt_fu
			lappend hash_alternative_fu_list $node_fu
		} else {
		    set idx [lsearch -index 0 $hash_alternative_fu_list $operation]
		    set new_fu_lst [lindex [lindex $hash_alternative_fu_list $idx] 1]
		    lappend new_fu_lst $alt_fu
		    lset node_fu 0 $operation
			lset node_fu 1 $new_fu_lst
		    lset hash_alternative_fu_list $idx $node_fu
		}
	}

	set i 0
	foreach hash $hash_alternative_fu_list {
		set j 0
		foreach alt_fu [lindex $hash 1] {
			foreach fast_fu $fast_fu_list {
				if {[get_attribute $alt_fu operation] == [get_attribute $fast_fu operation]} {
					lappend alt_fu [expr [get_attribute $fast_fu power] * [get_attribute $fast_fu delay] - [get_attribute $alt_fu power] * [get_attribute $alt_fu delay]]
					set hash_1 [lreplace [lindex $hash 1] $j $j $alt_fu]
					set hash [lreplace $hash 1 1 $hash_1]
					break
				}
			}
			incr j
		}
		set hash_alternative_fu_list [lreplace $hash_alternative_fu_list $i $i $hash]
		incr i
	}

	#****************************ALAP****************************************
	set continue_clock [clock milliseconds]
	set alap_schedule [list]
	set node_end_time [list]	

	set sorted_list [get_sorted_nodes]
	set rev_list [lreverse $sorted_list]

	foreach node $rev_list {
		set end_time $lambda
		foreach child [get_attribute $node children] {
			set child_op [get_attribute $child operation]
			foreach elem $fast_fu_list {
				if {[get_attribute $elem operation] eq $child_op} {
					set fu $elem
					break
				}
			}
			set child_delay [get_attribute $fu delay]
			set idx_child_end [lsearch -index 0 $node_end_time $child]
			set child_end_time [lindex [lindex $node_end_time $idx_child_end] 1]
			set child_start_time [expr {$child_end_time - $child_delay +1}]
			if { $child_start_time <= $end_time } {
				set end_time [expr $child_start_time -1]
			}
		}
		lappend node_end_time "$node $end_time"
	}

	foreach node $useful_attr_schedule {
		set node_id [lindex $node 0]
		set node_st [lindex $node 1]
		set node_fu [lindex $node 3]
		set node_delay [get_attribute $node_fu delay]
		set node_et [expr $node_st+$node_delay -1]
		foreach child [get_attribute $node_id children] {
			set child_index [lsearch -index 0 $useful_attr_schedule $child]
			if {$child_index >= 0} {
				set child_st [lindex $useful_attr_schedule $child_index 1]
				if {$child_st<=$node_et} {
					puts "$child inizia prima della fine di $node_id $child_st $node_et"
				}
			} else {
				return "manca $child figlio di $node_id"
			}
		}
	}

	#******************************LIST SCHEDULING MIN RESOURCES LATENCY CONSTRAINED*********************
	#output format: useful_attr_schedule
	#it is a list where each element is a list composed by : node_id, start time, slack and fu

	set list_schedule [list]
	set op_list [list]
	set start_time 1
	set num_nodes 0

	set sort_alap_schedule [lsort -integer -index 1 $alap_schedule]
	if {[lindex [lindex $sort_alap_schedule 0] 1] <= 0} {
		puts "UNFEASIBLE TIME CONSTRAINT!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
		set diff [expr {1 - [lindex [lindex $sort_alap_schedule 0] 1]} ]
		puts "The violation is of $diff time unit"
		return $list_schedule
	}

	#creation of a list containing all the operations and the corresponding HW resources used (initialized to 1)
	foreach node [get_sorted_nodes] {
		incr num_nodes
		set node_op [get_attribute $node operation]
		if {[lsearch -index 0 $op_list $node_op] == -1} {
			lappend op_list "$node_op 1"
		}
	}

	while {[llength $list_schedule] < $num_nodes} {
		
		set temp_list $list_schedule	
		#find the unfinished nodes
		set unfinished_op {}
		foreach op $op_list {
			foreach sched_node $temp_list {
				if { [get_attribute [lindex $sched_node 0] operation] eq [lindex $op 0]} {
					foreach elem $fast_fu_list {
						if {[get_attribute $elem operation] eq [lindex $op 0]} {
							set fu $elem
							break
						}
					}	
					set delay [get_attribute $fu delay]
					set node_end_time [expr { [lindex $sched_node 1] + $delay -1}]
					if { $node_end_time >= $start_time } {
						lappend unfinished_op [lindex $sched_node 0]
					}	
				}
			}
		}
		foreach pair $op_list {
			#find inside the unfinished nodes the ones of the current operation
			set num_unfinish 0
			foreach elem $unfinished_op {
				if {[get_attribute $elem operation] eq [lindex $pair 0]} {
					incr num_unfinish
				}
			}	
			#find the candidates
			set candidates {}
			set sorted_slack_list {}
			foreach node [get_sorted_nodes] {
				if {[get_attribute $node operation] eq [lindex $pair 0]} {
					set valid_candidate 1
					if {[lsearch -index 0 $temp_list $node] == -1 } {
						set node_parents [get_attribute $node parents]
						if {[llength $node_parents] > 0} {
							foreach parent $node_parents {
								if {([lsearch -index 0 $temp_list $parent] == -1) || ([lsearch $unfinished_op $parent] >=0)} {
									set valid_candidate 0
								} 
							}
							if {$valid_candidate > 0} {
								lappend candidates $node
							}
						} else {
							lappend candidates $node
						}
					}
				}
			}
			#schedule the candidates with slack = 0 and the ones that don't require additional resuorces
			set res 0
			foreach candidate $candidates {
				set alap_idx [lsearch -index 0 $alap_schedule $candidate]
				set alap_time [lindex [lindex $alap_schedule $alap_idx] 1]
				set candidate_slack [expr {$alap_time - $start_time}]
				lappend sorted_slack_list "$candidate $candidate_slack"
				if {$candidate_slack <= 0} {
					lappend list_schedule "$candidate $start_time"
					incr res
				}
			}
			set sorted_slack_list [lsort -integer -increasing -index 1  $sorted_slack_list]
			
			if {[expr {$res + $num_unfinish}] >= [lindex $pair 1]} {
				set new "[lindex $pair 0] [expr {$res + $num_unfinish}]"
				set idx [lsearch -index 0 $op_list [lindex $pair 0]]
				set op_list [lreplace $op_list $idx $idx $new]
			} else {
				set res_available [expr {[lindex $pair 1] - $res - $num_unfinish }]
				set i $res
				while {($i < [llength $sorted_slack_list]) && ($res_available > 0)} {
					lappend list_schedule "[lindex [lindex $sorted_slack_list $i] 0] $start_time"
					incr i
					set res_available [expr {$res_available -1}]
				}
			}
		}
		incr start_time
	}
	set init_power 0
	set init_area 0

	#compute the initial power
	foreach pair $list_schedule {
		set node_id [lindex $pair 0]
		set start_time [lindex $pair 1]
		foreach elem $fast_fu_list {
			if {[get_attribute $elem operation] eq [get_attribute $node_id operation]} {
				set init_power [expr {$init_power + double([get_attribute $elem power]*[get_attribute $elem delay])}]
			}
		}	
	}
	#compute the initial area
	foreach elem $op_list {
		set type [lindex $elem 0]
		set num [lindex $elem 1]
		foreach elem $fast_fu_list {
			if {[get_attribute $elem operation] eq $type} {
				set init_area [expr {$init_area + [get_attribute $elem area]*$num}]
			}
		}	
	}
	#this is the list that will be used and modified during the optimization algorithm in which each entry contains
	#the name of the node, its start time, its slack and its fu.
	set useful_attr_schedule [list]

	foreach elem $list_schedule {
		set node [lindex $elem 0]
		set start_time [lindex $elem 1]
		set alap_idx [lsearch -index 0 $alap_schedule [lindex $elem 0]]
		set alap_time [lindex [lindex $alap_schedule $alap_idx] 1]
		set slack [expr {$alap_time - [lindex $elem 1]}]
		foreach lib_fu $fast_fu_list {
			if {[get_attribute $lib_fu operation] eq [get_attribute $node operation]} {
				set fu $lib_fu
				break
			}
		}	
		lappend useful_attr_schedule "$node $start_time $slack $fu"
	}
	#controllo contains the initial schedule to check later if the first algorithm has applied any changes
	set controllo $useful_attr_schedule

	#*****************************SHARING*****************************************

	#output format :
	# fu_color_list: {{functional_unit_id} {color_list}}
	# color_list: {num_nodes {list_of_nodes}}

	set fu_color_list [list]
	set pattern_list {{} {}}
	foreach fu [get_lib_fus] {
		set l ""
		set match_idx [lsearch -index 3 -all $useful_attr_schedule $fu]
		foreach index $match_idx {
			lappend l [lindex $useful_attr_schedule $index]
		} 
		set l [lsort -integer -index 1 $l]
		set color 0
		set color_list [list]
		while {[llength $l] > 0} {
			set colored_nodes [list]
			set right_edge 0
			foreach node $l {
				if {[lindex $node 1] > $right_edge} {
					lappend colored_nodes $node
					set right_edge [expr {[lindex $node 1] + [get_attribute [lindex $node 3] delay] -1}]
					set idx [lsearch $l $node]
					set l [lreplace $l $idx $idx]
				}
			}
			incr color
			lappend color_list "[llength $colored_nodes] $colored_nodes"
		}
		if {[llength $color_list ] > 0} {
			lset pattern_list 0 $fu
			lset pattern_list 1 $color_list
			lappend fu_color_list $pattern_list
		}
	}
	set final_color_list $fu_color_list

	#***********************START OF THE FIRST OPTIMIZATION ALGORITHM*********************************
	#output format: useful_attr_schedule

	#scan the schedule looking at each type of fu used
	foreach type $fu_color_list {
		#TBC (to be changed) = flag used to indicate if the schedule has to be updated 
		set TBC 0
		set alternative_scheduling $useful_attr_schedule
		set node_fu [lindex $type 0]
		set node_op [get_attribute $node_fu operation]
		set node_delay [get_attribute $node_fu delay]
		set node_area [get_attribute $node_fu area]
		set i [lsearch -index 0 $hash_alternative_fu_list $node_op]
		set alternatives [lindex $hash_alternative_fu_list $i 1]
		#foreach color we look at the possible substitutions that can be applied and at the performance difference between the current fu (fast) and each alternative
		#the substitutions could be applied only if the slack is greater than the difference between the delay of the new fu and the current one
		#foreach color we selected the best substitution and, after having changed all the colors of the same type, we run again the list scheduling to obtain a new schedule
		foreach color [lindex $type 1] {
			set earn 1
			set node_to_be_changed ""
			#look at each alternative to choose the best one
			foreach alt $alternatives {
				set alternative [lindex $alt 0]
				set alt_delay [get_attribute $alternative delay]
				set num_nodes [lindex $color 0]
				set n_sostituzioni 0
				set new_schedule $useful_attr_schedule
				set possible_nodes ""
				#scanning of the color to compute the number of possible substitutions of fu
				for {set y 1} {$y<= $num_nodes} {incr y} {
					set node [lindex $color $y 0]
					set ind [lsearch -index 0 $useful_attr_schedule $node]
					if {$alt_delay <= [lindex $useful_attr_schedule $ind 2]} {
						incr n_sostituzioni
						lappend possible_nodes $node
					}
				}
				#two different computations depending if we are able to subtitute each node fu inside the color or not
				if {$n_sostituzioni!= $num_nodes} {
					set area_ratio [expr (double($init_area)+double([get_attribute $alternative area]))/double($init_area)]
				} else {
					set area_ratio [expr (double($init_area)+double([get_attribute $alternative area])- double($node_area))/double($init_area)]
				}
				set power_ratio [expr (double($init_power)- double([lindex $alt 1])*$n_sostituzioni)/double($init_power)]
				set product [expr double($area_ratio)*double($power_ratio)]
				#if we have a gain we save the current alternative and the nodes whose fu will be changed in the scheduling
				if {$product< $earn } {
					set earn $product
					set node_to_be_changed $possible_nodes
					set best_alt $alternative
				}
			}
			#modify the fu on the selected nodes (if there are any)
			foreach node_TBC $node_to_be_changed {
				set index [lsearch -index 0 $alternative_scheduling $node_TBC]
				set new_row [lreplace [lindex $alternative_scheduling $index] 3 3 $best_alt]
				set alternative_scheduling [lreplace $alternative_scheduling $index $index $new_row]
				set TBC 1
			}
		}	
		#if there are possible substitutions, run the list scheduling min resources again
		if {$TBC}	{			
			#****************************ALAP****************************************
			set alap_schedule [list]
			set node_end_time [list]	

			set sorted_list [get_sorted_nodes]
			set rev_list [lreverse $sorted_list]

			foreach node $rev_list {
				set end_time $lambda
				foreach child [get_attribute $node children] {
					set child_index [lsearch -index 0 $alternative_scheduling $child]
					set fu [lindex $alternative_scheduling $child_index 3]
					set child_delay [get_attribute $fu delay]
					set idx_child_end [lsearch -index 0 $node_end_time $child]
					set child_end_time [lindex [lindex $node_end_time $idx_child_end] 1]
					set child_start_time [expr {$child_end_time - $child_delay} +1]
					if { $child_start_time <= $end_time } {
						set end_time [expr {$child_start_time -1}]
					}
				}
				lappend node_end_time "$node $end_time"
			}

			foreach pair $node_end_time {
				set node_id [lindex $pair 0]
				set node_index [lsearch -index 0 $alternative_scheduling $node_id]
				set fu [lindex $alternative_scheduling $node_index 3]
				set node_delay [get_attribute $fu delay]
				set node_start [expr {[lindex $pair 1] - $node_delay +1}]
				lappend alap_schedule "$node_id $node_start"
			}
			
			#******************************LIST SCHEDULING MIN RESOURCES LATENCY CONSTRAINED*********************
			set list_schedule [list]
			set fu_list [list]
			set start_time 1
			set num_nodes 0
			set flag 1
			set sort_alap_schedule [lsort -integer -index 1 $alap_schedule]
			if {[lindex [lindex $sort_alap_schedule 0] 1] <= 0} {
				set flag 0
				set diff [expr {1 - [lindex [lindex $sort_alap_schedule 0] 1]} ]
			}
			#if we are not overpassing the constraint with the substitutions that we have made, run the scheduling algorithm
			if {$flag} {
				foreach node [get_sorted_nodes] {
					incr num_nodes
					set node_index [lsearch -index 0 $alternative_scheduling $node] 
					set node_f [lindex $alternative_scheduling $node_index 3]
					if {[lsearch -index 0 $fu_list $node_f] == -1} {
						lappend fu_list "$node_f 1"
					}
				}
				while {[llength $list_schedule] < $num_nodes} {
					set temp_list $list_schedule
					set unfinished_fu {}
					foreach fu $fu_list {
						foreach sched_node $temp_list {
							set sched_node_index [lsearch -index 0 $alternative_scheduling [lindex $sched_node 0]] 
							set sched_node_f [lindex $alternative_scheduling $sched_node_index 3]
							if { $sched_node_f eq [lindex $fu 0]} {
								set delay [get_attribute [lindex $fu 0] delay]
								set node_end_time [expr { [lindex $sched_node 1] + $delay -1}]
								if { $node_end_time >= $start_time } {
									lappend unfinished_fu [lindex $sched_node 0]
								}	
							}
						}
					}
					foreach pair $fu_list {
						set num_unfinish 0
						foreach elem $unfinished_fu {
							set elem_index [lsearch -index 0 $alternative_scheduling $elem] 
							set elem_fu [lindex $alternative_scheduling $elem_index 3]
							if {$elem_fu eq [lindex $pair 0]} {
								incr num_unfinish
							}
						}
						set candidates {}
						set sorted_slack_list {}
						foreach node [get_sorted_nodes] {
							set node_index [lsearch -index 0 $alternative_scheduling $node] 
							set node_f [lindex $alternative_scheduling $node_index 3]
							
							if {$node_f eq [lindex $pair 0]} {
								set valid_candidate 1
								if {[lsearch -index 0 $temp_list $node] == -1 } {
									set node_parents [get_attribute $node parents]
									if {[llength $node_parents] > 0} {
										foreach parent $node_parents {
											if {([lsearch -index 0 $temp_list $parent] == -1) || ([lsearch $unfinished_fu $parent] >=0)} {
												set valid_candidate 0
											} 
										}
										if {$valid_candidate > 0} {
											lappend candidates $node
										}
									} else {
										lappend candidates $node
									}
								}
							}
						}
						set res 0
						foreach candidate $candidates {
							set alap_idx [lsearch -index 0 $alap_schedule $candidate]
							set alap_time [lindex [lindex $alap_schedule $alap_idx] 1]
							set candidate_slack [expr {$alap_time - $start_time}]
							lappend sorted_slack_list "$candidate $candidate_slack"
							if {$candidate_slack <= 0} {
								lappend list_schedule "$candidate $start_time"
								incr res
							}
						}
						set sorted_slack_list [lsort -integer -increasing -index 1  $sorted_slack_list]
						
						if {[expr {$res + $num_unfinish}] >= [lindex $pair 1]} {
							set new "[lindex $pair 0] [expr {$res + $num_unfinish}]"
							set idx [lsearch -index 0 $fu_list [lindex $pair 0]]
							set fu_list [lreplace $fu_list $idx $idx $new]
						} else {
							set res_available [expr {[lindex $pair 1] - $res - $num_unfinish }]
							set i $res
							while {($i < [llength $sorted_slack_list]) && ($res_available > 0)} {
								lappend list_schedule "[lindex [lindex $sorted_slack_list $i] 0] $start_time"
								incr i
								set res_available [expr {$res_available -1}]
							}
						}
					}
					incr start_time
				}
				set new_power 0
				set new_area 0

				foreach pair $list_schedule {
					set node_id [lindex $pair 0]
					set start_time [lindex $pair 1]
					set node_index [lsearch -index 0 $alternative_scheduling $node_id] 
					set node_f [lindex $alternative_scheduling $node_index 3]
					set new_power [expr {$new_power + double([get_attribute $node_f power]*[get_attribute $node_f delay])}]
				}
				foreach elem $fu_list {
					set num [lindex $elem 1]
					set new_area [expr {$new_area + [get_attribute $elem area]*$num}]
				}
				
				set area_ratio [expr double($new_area)/double($init_area)]
				set power_ratio [expr double($new_power)/double($init_power)]
				
				if {[expr double($area_ratio)*double($power_ratio)] < 1} {
					set init_area $new_area
					set init_power $new_power
					set useful_attr_schedule [list]

					foreach elem $list_schedule {
						set node [lindex $elem 0]
						set start_time [lindex $elem 1]
						set alap_idx [lsearch -index 0 $alap_schedule [lindex $elem 0]]
						set alap_time [lindex [lindex $alap_schedule $alap_idx] 1]
						set slack [expr {$alap_time - [lindex $elem 1]}]
						set node_index [lsearch -index 0 $alternative_scheduling $node] 
						set fu [lindex $alternative_scheduling $node_index 3]
						lappend useful_attr_schedule "$node $start_time $slack $fu"
					}
					# SHARING
					set fu_color_list [list]
					set pattern_list {{} {}}

					foreach fu [get_lib_fus] {
						set l ""
						set match_idx [lsearch -index 3 -all $useful_attr_schedule $fu]
						foreach index $match_idx {
							lappend l [lindex $useful_attr_schedule $index]
						}
						set l [lsort -integer -index 1 $l]
						set color 0
						set color_list [list]
						
						while {[llength $l] > 0} {
							set colored_nodes [list]
							set right_edge 0
							foreach node $l {
								if {[lindex $node 1] > $right_edge} {
									lappend colored_nodes $node
									set right_edge [expr {[lindex $node 1] + [get_attribute [lindex $node 3] delay] -1}]
									set idx [lsearch $l $node]
									set l [lreplace $l $idx $idx]
								}
							}
							incr color
							lappend color_list "[llength $colored_nodes] $colored_nodes"
						}
						if {[llength $color_list ] > 0} {
							lset pattern_list 0 $fu
							lset pattern_list 1 $color_list
							lappend fu_color_list $pattern_list
						}
					}
					set final_color_list $fu_color_list
				}
			}
		}		
	} 
	#START OF THE SECOND ALGORITHM
	#output format: useful_attr_schedule

	#this algorithm will be run only if the first algorithm didn't apply any modification
	if {$useful_attr_schedule eq $controllo} {
		puts "secondo algoritmo"
		set num_nodes [llength $useful_attr_schedule]
		#foreach node we try to substitute its fu and the one of the nodes in the graph that are compatible to it
		for {set i 0} {$i < [expr {$num_nodes -1}]} {incr i} {
			set node_name [lindex [lindex $useful_attr_schedule $i] 0]
			set node_op [get_attribute $node_name operation]
			set alternative [lsearch -inline -index 0 $hash_alternative_fu_list $node_op]
			set list_alternative [lindex $alternative 1] 
			set node_slack [lindex [lindex $useful_attr_schedule $i] 2]
			#we look at all possible alternatives
			foreach new_fu $list_alternative {
				set node_fu [lindex [lindex $useful_attr_schedule $i] 3]
				set new_fu_delay [get_attribute $new_fu delay]
				set fu_delay_diff [expr {$new_fu_delay - [get_attribute $node_fu delay]}]
				#if the substitution is possible we do it and update the subgraph
				if {$fu_delay_diff <= $node_slack} {
					set new_schedule [sub_graph_change $i $i $useful_attr_schedule $fu_delay_diff $new_fu]
					if {$new_schedule == 0} {
						break;
					}
					set node_new_attr "[lindex [lindex $new_schedule $i] 0] [lindex [lindex $new_schedule $i] 1] [lindex [lindex $new_schedule $i] 2] $new_fu"
					set new_schedule [lreplace $new_schedule $i $i $node_new_attr]
					set node_end [expr {[lindex $new_schedule $i 1] + $new_fu_delay}]
					#search for compatible nodes whose fu could be changed
					for {set j [expr {$i +1}]} {$j < [expr {$num_nodes -1}]} {incr j} {
						if {[lindex $new_schedule $j 1] >= $node_end} {
							set j_fu [lindex [lindex $new_schedule $j] 3]
							if {[get_attribute $j_fu operation] eq [get_attribute $node_fu operation]} {
								set other_slack [lindex [lindex $new_schedule $j] 2]
								set fu_delay_diff [expr {$new_fu_delay - [get_attribute $j_fu delay]}]
								if {$fu_delay_diff >= $new_fu_delay} {
									#set fu_delay_diff [expr {$new_fu_delay - [get_attribute $j_fu delay]}]
									set new_schedule [sub_graph_change $j $j $new_schedule $fu_delay_diff $new_fu]
									if {$new_schedule == 0} {
										break;
									}
									set node_new_attr "[lindex [lindex $new_schedule $j] 0] [lindex [lindex $new_schedule $j] 1] [lindex [lindex $new_schedule $j] 2] $new_fu"
									set new_schedule [lreplace $new_schedule $j $j $node_new_attr]
									set node_end [expr {[lindex $new_schedule $j 1] + $new_fu_delay}]
								}
							}
						}
					}
					if {$new_schedule != 0} {
						# SHARING
						set fu_color_list [list]
						set pattern_list {{} {}}
						foreach fu [get_lib_fus] {
							set l ""
							set match_idx [lsearch -index 3 -all $new_schedule $fu]
							foreach index $match_idx {
								lappend l [lindex $new_schedule $index]
							} 
							set l [lsort -integer -index 1 $l]
							set color 0
							set color_list [list]
							while {[llength $l] > 0} {
								set colored_nodes [list]
								set right_edge 0
								foreach node $l {
									if {[lindex $node 1] > $right_edge} {
										lappend colored_nodes $node
										set right_edge [expr {[lindex $node 1] + [get_attribute [lindex $node 3] delay] -1}]
										set idx [lsearch $l $node]
										set l [lreplace $l $idx $idx]
									}
								}
								incr color
								lappend color_list "[llength $colored_nodes] $colored_nodes"
							}
							if {[llength $color_list ] > 0} {
								lset pattern_list 0 $fu
								lset pattern_list 1 $color_list
								lappend fu_color_list $pattern_list
							}
						}
						# METRICS EVALUATION
						# after the changes applied we recompute the new power and delay
						set new_area 0
						set new_power 0
						set new_cost 0
						foreach type $fu_color_list {
							set op [lindex $type 0]
							set op_area [get_attribute $op area]
							set op_power [get_attribute $op power]
							set op_delay [get_attribute $op delay]
							set new_area [expr {$new_area + $op_area* [llength [ lindex $type 1]]}]
							foreach color [lindex $type 1] {
								set new_power [expr {$new_power + double($op_power*$op_delay*[lindex $color 0])}]
							}
						}
						set new_cost [expr {sqrt([pow $new_power 2] + [pow $new_area 2])}]
						# CONFRONTO
						#if the new product between the ratio of the power and the ratio of the area is lower than the previous one we mantain the new schedule
						set ratio_area [expr {double($new_area) / double($init_area)}]
						set ratio_power [expr {double($new_power) / double($init_power)}]
						if {[expr {$ratio_area * $ratio_power}] < 1} {
							set init_cost $new_cost
							set init_area $new_area
							set init_power $new_power
							set useful_attr_schedule $new_schedule
							set final_color_list $fu_color_list
						}
					}
				}
			}
		}	
	}
  puts "ciao"
  #Return the output lists.
  set out_schedule [list]
  set out_fu_list [list]
  set out_color_list [list]

  foreach node $useful_attr_schedule {
    set node_id [lindex $node 0] 
    set node_fu [lindex $node 3]
    set node_start_time [lindex $node 1]
    lappend out_schedule "$node_id $node_start_time"
    lappend out_fu_list "$node_id $node_fu"
  }
  #EndFor.
  foreach element $final_color_list {
    set fu_name [lindex $element 0]
    set num_allocated [llength [lindex $element 1] ]
    lappend out_color_list "$fu_name $num_allocated"
  }

  set output { {} {} {} }
  lset output 0 $out_schedule
  lset output 1 $out_fu_list
  lset output 2 $out_color_list
  
  foreach elem $useful_attr_schedule {
		set fu_delay [get_attribute [lindex $elem 3] delay]
		set node_end [expr {[lindex $elem 1] + $fu_delay -1}]
		if {$node_end > $lambda} {
			puts "DAVIDE E' UNFEASIBLE !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
		}
	}
	foreach node $useful_attr_schedule {
		set node_id [lindex $node 0]
		set node_st [lindex $node 1]
		set node_fu [lindex $node 3]
		set node_delay [get_attribute $node_fu delay]
		set node_et [expr $node_st+$node_delay -1]
		foreach child [get_attribute $node_id children] {
			set child_index [lsearch -index 0 $useful_attr_schedule $child]
			if {$child_index >= 0} {
				set child_st [lindex $useful_attr_schedule $child_index 1]
				if {$child_st<=$node_et} {
					puts "$child inizia prima della fine di $node_id $child_st $node_et"
				}
			} else {
				return "manca $child figlio di $node_id"
			}
		}
	}
  
  return $output
}