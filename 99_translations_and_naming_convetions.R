translate_strings <- function(input_strings) {
  translation_map <- list(
    "btr" = "BTR",
    "spor" = "Track",
    "event (date)" = "Date",
    "fra" = "From",
    "til" = "To", 
    "ja"= "Yes", 
    "nej" = "No",
    "v" = "Left", 
    "h" = "Right", 
    "venstre"= "Left", 
    "højre" = "Right",
    "skinnestreng" = "Rail_string",
    "spsk nr." = "Switch_nr.",
    "spsk" = "Switch",
    "fejlid" = "Defect_ID",
    "ren skinne" = "Clean_rail",
    "ingen fejl" = "No_defect",
    "fejllængde" = "Defect_length", 
    "fejlbredde" = "Defect_width",
    "dybde fra" = "Depth_from", 
    "dybde til" = "Depth_to",
    "fejlgruppe" = "Defect_group",
    "fejl fundet i" = "Defect_found_in",
    "sporkonstruktion" = "Track_type",
    "synlig" = "Visible",
    "kombineret fejl" = "Combined_defect",
    "under grænsen" = "Under_limit",
    "skinneprofil" = "Profile",
    "kurveforhold" = "Curvature",
    "kurve" = "Curve",
    "overgangskurve" = "Transition_curve",
    "år" = "Year",
    "fjernet" = "Removed",
    "genmålt" = "Remeasured",
    "åben" = "Open", 
    "sikret" = "Secured",
    "højreskifte" = "Switch_to_right",
    "venstreskifte" = "Switch_to_left",
    "krydsningssporskifte" = "Switch_crossing",
    "udv. skinne" = "Outer_rail",
    "indv. skinne" = "Inner_rail",
    "ret spor" = "Straight_track",
    "stålkvalitet" = "Steel",
    "befæstelse" = "Fastening",
    "spsk type" = "Switch_type", 
    "state" = "State", 
    "fejlgruppe calc." = "Defect_group_calc", 
    "uic code" = "UIC",
    "thermitsvejsning" = "Aluminothermic_welding",
    "pålægssvejsning" = "Repair_weld",
    "brændstuksvejsning" = "Flash_butt_welding",
    "laskekammer" = "Fish_plate_chamber",
    "boret hul" = "Drilled_hole",
    "ukendt" = "Unknown",
    "sideskinne" = "Side_rail",
    "overgangsskinne/svejsning" = "Transition_rail_weld",
    "sporskiftetunge" = "Switch_blade",
    "alm. spor" = "Standard_track",
    "krydsning" = "Crossing",
    "sporskifte" = "Switch",
    "x-sporskifte" = "X_switch",
    "skinneudtræk" = "Rail_pullout",
    "transversal i togvejsspor" = "Crossover_in_track",
    "vigespor" = "Siding",
    "sidespor" = "Branch_track", 
    "gennemgående spor, ikke gennemkørselsspor" = "Through_nonrunning_track",
    "hovedspor" = "Main_track", 
    "gennemgående spor, gennemkørselsspor" = "Through_running_track",
    "spor på private baner" = "Private_track",
    "Spsk" = "Switch",  
    "uic60" = "UIC60",  
    "dsb45" = "DSB45",  
    "dsb37" = "DSB37",  
    "dsb60" = "DSB60",  
    "rillesk. øvrige" = "Grooved_rail_other",  
    "ovk" = "Level_crossing",  
    "dsb32" = "DSB32",  
    "hi-54" = "HI-54",
    "na" = "NA"
  )
  
  translation <- sapply(input_strings, function(input_string) {
    if (is.na(input_string)) {
      return(NA)  # Preserve NA values
    }
    
    # Check if the input string is already in English, if so, return it as-is
    lower_input <- tolower(input_string)
    if (lower_input %in% tolower(as.vector(unlist(translation_map)))) {
      return(input_string)  # No translation needed if already in English
    }
    
    # Otherwise, translate if possible
    if (!is.null(translation_map[[lower_input]])) {
      return(translation_map[[lower_input]])
    } else {
      return("Unknown translation")
    }
  })
  
  return(as.vector(translation))
}

