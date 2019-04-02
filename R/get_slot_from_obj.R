get_slot_from_obj <- function(obj,
                              slot_name) {
  current_obj_slot <- NULL
  # Test if the slot exists ----
  if (slot_name == "base") {
    # Check the base slot and the object type
    if (inherits(obj, "data.frame")) {
      # obj is a data frame
      current_obj_slot <- obj
    } else {
      if (inherits(obj, "list")) {
        # obj is a list
        if ("base" %in% names(obj)) {
          current_obj_slot <- obj[["base"]]
        }
      } else {
        # another S3/S4 obj
        current_obj_slot <- obj
      }
    }
  } else {
    # Check another slot and the object type
    if (inherits(obj, "list")) {
      # obj is a list
      if (slot_name %in% names(obj)) {
        current_obj_slot <- obj[[slot_name]]
      }
    } else {
      # Another S3/S4 obj
      if (slot_name %in% slotNames(obj)) {
        current_obj_slot <- slot(obj, slot_name)
      }
    }
  }
  return(current_obj_slot)
}
