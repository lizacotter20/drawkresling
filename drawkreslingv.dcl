drawkresling : dialog { label = "Draw Kresling"; 
 
	: column {
		: row {
			: boxed_column {label = "Insertion Point";

		: edit_box { 
						key = "H"
						label = "Enter deployed height:"
						edit_width = 10;
						value = "";
						alignment = right;
				}
		: edit_box { 
						key = "H0"
						label = "Enter folded height:"
						edit_width = 10;
						value = "";
						alignment = right;
				}
		: edit_box { 
						key = "n"
						label = "Enter number of polygon edges:"
						edit_width = 10;
						value = "";
						alignment = right;
				}
		: edit_box { 
						key = "b"
						label = "Enter length of each polygon edge:
						edit_width = 10;
						value = "";
						alignment = right;
				}
			}
		}
	}
 
}