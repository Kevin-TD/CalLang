let read_file filename =
  let channel = open_in filename in
  let size = in_channel_length channel in
  let contents = really_input_string channel size in
  close_in channel;
  contents

let write_to_file filename content =
  let oc = open_out filename in  
  output_string oc content;
  close_out oc