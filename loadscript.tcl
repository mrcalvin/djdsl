package ifneeded djdsl::v1e 0.1 {
  source v1e.tcl
}

package ifneeded djdsl::ctx 0.1 {
  source ctx.tcl
}

package ifneeded djdsl::lm 0.1 {
  source lm.tcl
}

package ifneeded djdsl::opeg 0.1 {
  source opeg.tcl
}

package ifneeded djdsl::dada 0.1 {
  source dada.tcl
}

package ifneeded djdsl::examples::models 0.1 {
  source [file join examples models.tcl]
}



# Local variables:
#    mode: tcl
#    tcl-indent-level: 2
#    indent-tabs-mode: nil
# End:
