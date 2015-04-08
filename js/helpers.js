// TODO: These should have proper bounds checks.

//Provides: caml_blit_string_to_bigstring
//Requires: caml_string_unsafe_get
function caml_blit_string_to_bigstring(buf1, ofs1, buf2, ofs2, len) {
	for (var i=0; i<len; i++) {
		buf2.set1(ofs2 + i, caml_string_unsafe_get(buf1, ofs1 + i));
	}
}

//Provides: caml_blit_bigstring_to_string
//Requires: caml_string_unsafe_set
function caml_blit_bigstring_to_string(buf1, ofs1, buf2, ofs2, len) {
	for (var i=0; i<len; i++) {
		caml_string_unsafe_set(buf2, ofs2 + i, buf1.get1(ofs1 + i));
	}
}

//Provides: caml_blit_bigstring_to_bigstring
function caml_blit_bigstring_to_bigstring(buf1, ofs1, buf2, ofs2, len) {
	for (var i=0; i<len; i++) {
		buf2.set1(ofs2 + i, buf1.get1(ofs1 + i));
	}
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
