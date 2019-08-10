// TODO: These should have proper bounds checks.

//Provides: bin_prot_get_float_offset
//Requires: caml_float_of_bytes, caml_ba_get_1
// (rescued from
//  https://github.com/ocsigen/js_of_ocaml/commit/347bced8f43000f63ada887151381e7a64140578)
function bin_prot_get_float_offset(a,p){
  var t = new Array(8);;
  for (var i = 0;i < 8;i++) t[7-i] = caml_ba_get_1(a,p++);
  var v = caml_float_of_bytes (t);
  return [254,v];
}

//Provides: bin_prot_blit_string_buf_stub
//Requires: caml_blit_string_to_bigstring
function bin_prot_blit_string_buf_stub(ofs1, buf1, ofs2, buf2, len) {
	caml_blit_string_to_bigstring(buf1, ofs1, buf2, ofs2, len)
}

//Provides: bin_prot_blit_buf_string_stub
//Requires: caml_blit_bigstring_to_string
function bin_prot_blit_buf_string_stub(ofs1, buf1, ofs2, buf2, len) {
	caml_blit_bigstring_to_string(buf1, ofs1, buf2, ofs2, len)
}

//Provides: bin_prot_blit_buf_stub
//Requires: caml_ba_sub, caml_ba_blit
function bin_prot_blit_buf_stub(ofs1, buf1, ofs2, buf2, len) {
	var src = caml_ba_sub(buf1, ofs1, len)
	var dst = caml_ba_sub(buf2, ofs2, len)
	caml_ba_blit(src, dst)
}
