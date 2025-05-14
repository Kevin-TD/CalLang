(** Defines values that are defaulted to if the '*' keyword is used for a MAKE statement *)

let month = 1
let day = 1
let year = (Unix.localtime (Unix.time ())).Unix.tm_year + 1900
let start_time = Hm_type.HrsMins (0, 0)
let end_time = Hm_type.HrsMins (1, 0)
let title = ""
let location = ""
let description = ""
let reminders = []
let job_color_id = "1"