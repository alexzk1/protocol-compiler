// Fermilab Software Legal Information (BSD License)
// Copyright (c) 2008-2019, Fermi Research Alliance, LLC
// All rights reserved.
//
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions
// are met:
//
// Redistributions of source code must retain the above copyright
// notice, this list of conditions and the following disclaimer.
//
// Redistributions in binary form must reproduce the above copyright
// notice, this list of conditions and the following disclaimer in the
// documentation and/or other materials provided with the
// distribution.
//
// Neither the name of the FERMI NATIONAL ACCELERATOR LABORATORY, nor
// the names of its contributors may be used to endorse or promote
// products derived from this software without specific prior written
// permission.
//
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
// "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
// LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
// FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
// COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,
// INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
// (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
// SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
// HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
// STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
// ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED
// OF THE POSSIBILITY OF SUCH DAMAGE.
//

#include <fstream>
#include <cassert>
#include <cctype>
#include <algorithm>
#include "pc.h"

static std::string xlatType(Struct const&);
static std::string type_name(Protocol const&, Field const&, bool = false);

static std::string toSnake(std::string const& o)
{
    std::string tmp;

    tmp.push_back(tolower(o[0]));
    for (size_t ii = 1; ii < o.size(); ++ii) {
	if (o[ii - 1] != '_' && o[ii] >= 'A' && o[ii] <= 'Z')
	    tmp.push_back('_');
	tmp.push_back(tolower(o[ii]));
    }
    return tmp;
}

static std::string toTypeName(std::string const& o)
{
    std::string tmp;

    tmp.push_back(toupper(o[0]));
    for (size_t ii = 1; ii < o.size(); ++ii)
	if (o[ii] != '_')
	    tmp.push_back(o[ii - 1] == '_' ? toupper(o[ii]) : o[ii]);
    return tmp;
}

static std::string enumName(std::string const& o)
{
    return toTypeName(o) + "Enum";
}

static std::string structName(std::string const& o)
{
    return toTypeName(o) + "Struct";
}

static std::string xlatType(Struct const& s)
{
    if (s.name == "bool")
	return "bool";
    else if (s.name == "int16")
	return "i16";
    else if (s.name == "int32")
	return "i32";
    else if (s.name == "int64")
	return "i64";
    else if (s.name == "double")
	return "f64";
    else if (s.name == "string")
	return "String";
    else if (s.name == "binary")
	return "Vec<u8>";
    else if (s.use == Struct::asEnum)
	return enumName(s.name);
    else
	return structName(s.name);
}

static std::string type_name(Protocol const& p, Field const& f,
			     bool const filterOptionals)
{
    Struct const& s = p.findType(f.type);
    std::string tmp = f.array ? "Vec<" + xlatType(s) + ">" : xlatType(s);

    if (!filterOptionals && f.optional)
	tmp = "Option<" + tmp + ">";
    return tmp;
}

static void dumpEnums(std::ostream& os, Protocol const& p)
{
    for (StructList::const_iterator ii = p.types.begin(); ii != p.types.end();
	 ++ii)
	if (ii->use == Struct::asEnum) {
	    std::string const name = enumName(ii->name);

	    os <<
		"/* This section defines and implements the '" <<
		ii->name << "'\n"
		"   enumeration of the protocol. */\n"
		"\n"
		"pub enum " << name << " {\n";

	    for (FieldList::const_iterator jj = ii->fields.begin();
		 jj != ii->fields.end(); ++jj)
		os << "  " << toTypeName(jj->name) << ",\n";

	    os <<
		"}\n\n"
		"impl Xlat for " << name << " {\n"
		"  // Adds the raw representation of the enumeration to\n"
		"  // the end of the binary (Vec<u8>).\n"
		"\n"
		"  #[allow(dead_code)]\n"
		"  fn to_buf(&self, buf: &mut Vec<u8>) -> () {\n"
		"    int_to_buf(0x80u8,\n"
		"\t       match self {\n";

	    for (FieldList::const_iterator jj = ii->fields.begin();
		 jj != ii->fields.end(); ++jj) {
		os <<
		    "\t\t " << name << "::" <<
		    toTypeName(jj->name) << " => " << jj->hash << "i64,\n";
	    }

	    os <<
		"\t       }, buf)\n"
		"  }\n\n"
		"  // This function tries to read one of the raw enumeration\n"
		"  // values from the iterator and returns the corresponding\n"
		"  // enumeration in a Some() value. Otherwise return None.\n"
		"\n"
		"  #[allow(dead_code)]\n"
		"  fn from_iter(iter: &mut Iterator<Item=u8>) -> "
		"Option<Box<" << name << ">> {\n"
		"    match get_int(0x80, iter) {\n";

	    for (FieldList::const_iterator jj = ii->fields.begin();
		 jj != ii->fields.end(); ++jj)
		os <<
		    "      Some(" << jj->hash << "i64) => Some(Box::new(" <<
		    name << "::" << toTypeName(jj->name) << ")),\n";

	    os <<
		"      _ => None\n"
		"    }\n"
		"  }\n"
		"}\n\n";
	}
}

static void dumpStructs(std::ostream& os, Protocol const& p)
{
    for (StructList::const_iterator ii = p.types.begin(); ii != p.types.end();
	 ++ii)
	if (ii->use == Struct::asStruct) {
	    std::string const name = xlatType(*ii);
	    size_t n_optional = 0;

	    os <<
		"/* This section defines and implements the '" <<
		ii->name << "'\n"
		"   structure type of the protocol. */\n"
		"\n"
		"#[derive(Default, Debug)]\n"
		"pub struct " << name << " {\n";

	    for (FieldList::const_iterator jj = ii->fields.begin();
		 jj != ii->fields.end(); ++jj) {
		os << "  pub " << toSnake(jj->name) << ": " <<
		    type_name(p, *jj) << ",\n";
		if (jj->optional)
		    ++n_optional;
	    }

	    os <<
		"}\n\n"
		"impl" << " Xlat for " << name << " {\n\n"
		"  // Adds the raw representation of the structure to\n"
		"  // the end of the binary (Vec<u8>).\n"
		"\n"
		"  #[allow(dead_code)]\n"
		"  fn to_buf(&self, buf: &mut Vec<u8>) -> () {\n"
		"    let n_fields = " << (ii->fields.size() * 2);

	    for (FieldList::const_iterator jj = ii->fields.begin();
		 jj != ii->fields.end(); ++jj)
		if (jj->optional)
		    os <<
			" -\n\t\t   "
			"(if self." << toSnake(jj->name) <<
			" == None { 2 } else { 0 })";
	    os <<
		";\n\n"
		"    // Add array tag and specify number of fields.\n\n"
		"    int_to_buf(0x50u8, n_fields, buf);\n";

	    for (FieldList::const_iterator jj = ii->fields.begin();
		 jj != ii->fields.end(); ++jj) {
		std::string const fldName = toSnake(jj->name);

		if (jj->optional)
		    os <<
			"\n"
			"    // Marshal '" << fldName << "' field.\n\n"
			"    if let Some(v) = &self." << fldName << " {\n"
			"      int_to_buf(0x10u8, " << jj->hash << ", buf);\n"
			"      Xlat::to_buf(v, buf);\n"
			"    }\n";
		else
		    os <<
			"\n"
			"    // Marshal '" << fldName << "' field.\n\n"
			"    int_to_buf(0x10u8, " << jj->hash << ", buf);\n"
			"    Xlat::to_buf(&self." << fldName << ", buf);\n";
	    }

	    os <<
		"  }\n\n"
		"  // Tries to decode the contents of the structure from the\n"
		"  // iterator. If the structure can be re-assenbled, this\n"
		"  // function returns Some(Box(data)). If any error occurs,\n"
		"  // it returns None.\n"
		"\n"
		"  #[allow(dead_code)]\n"
		"  fn from_iter(iter: &mut Iterator<Item=u8>) -> Option<Box<" <<
		xlatType(*ii) << ">> {\n"
		"    match get_length(0x50, iter) {\n"
		"      Some(n) if n % 2 == 0 => {\n";

	    for (FieldList::const_iterator jj = ii->fields.begin();
		 jj != ii->fields.end(); ++jj)
		os <<
		    "\tlet mut tmp_" << toSnake(jj->name) << ": " <<
		    type_name(p, *jj) << " = Default::default();\n";

	    size_t const n_required = ii->fields.size() - n_optional;

	    if (n_required > 0)
		for (size_t ii = 0; ii <= n_required / 32; ++ii) {
		    size_t const bits =
			32 - std::min(size_t(n_required - ii * 32),
				      size_t(32));

		    os <<
			"\tlet mut req_fld" << ii << ": u32 = 0x" <<
			std::hex << (0xffffffffu >> bits) << std::dec <<
			"u32;\n";
		}

	    os <<
		"\n"
		"\tfor _ in 0..(n / 2) {\n"
		"\t  if let Some(fld) = <i16 as Xlat>::from_iter(iter) {\n"
		"\t    match *fld {\n";

	    size_t req_count = 0;

	    for (FieldList::const_iterator jj = ii->fields.begin();
		 jj != ii->fields.end(); ++jj) {
		os <<
		    "\t      " << jj->hash << " => \n"
		    "\t\t  if let Some(v) = <" <<
		    type_name(p, *jj, true) << " as Xlat>::from_iter(iter) {\n"
		    "\t\t    tmp_" << toSnake(jj->name) <<
		    " = " << (jj->optional ? "Some(*v)" : "*v" ) << ";\n";
		if (!jj->optional) {
		    os <<
			"\t\t    req_fld" << (req_count / 32) <<
			" &= !0x" << std::hex << (1u << (req_count % 32)) <<
			std::dec << "\n";
		    ++req_count;
		}

		os <<
		    "\t\t  } else {\n"
		    "\t\t    return None\n"
		    "\t\t  },\n";
	    }

	    os <<
		"\t      _ =>\n"
		"\t\t  return None\n"
		"\t    }\n"
		"\t  } else {\n"
		"\t    return None\n"
		"\t  }\n"
		"\t}\n"
		"\n";

	    if (n_required > 0) {
		os <<
		    "\t// Make sure all the required fields have been\n"
		    "\t// provided by the iterator.\n\n";

		for (size_t ii = 0; ii <= n_required / 32; ++ii)
		    os <<
			(ii == 0 ? "\tif " : " ||\n\t   ") <<
			"req_fld" << ii << " != 0";

		os <<
		    " {\n"
		    "\t  return None\n"
		    "\t}\n\n";
	    }

	    os <<
		"\tSome(Box::new(" << xlatType(*ii) << " { ";

	    std::string const indent = std::string(25 + name.length(), ' ');

	    for (FieldList::const_iterator jj = ii->fields.begin();
		 jj != ii->fields.end(); ++jj) {
		std::string const fldName = toSnake(jj->name);

		if (jj != ii->fields.begin())
		    os << ",\n" << indent;
		os <<
		    fldName << ": tmp_" << fldName;
	    }

	    os <<
		" }))\n"
		"      }\n"
		"      _ => None\n"
		"    }\n"
		"  }\n"
		"}\n\n";
	}
}

static void dumpSingleMessage(std::string const& tn, std::ostream& os,
			      Protocol const& p, Struct const& s)
{
    std::string const name = toTypeName(s.name);
    size_t n_optional = 0;

    for (FieldList::const_iterator jj = s.fields.begin();
	 jj != s.fields.end(); ++jj)
	if (jj->optional)
	    ++n_optional;

    os <<
	"\t" << int16_t(s.hash) << " => {\n"
	"\t  match get_length(0x50, iter) {\n"
	"\t    Some(n) if n % 2 == 0 => {\n";

    for (FieldList::const_iterator jj = s.fields.begin();
	 jj != s.fields.end(); ++jj)
	os <<
	    "\t      let mut tmp_" << toSnake(jj->name) << ": " <<
	    type_name(p, *jj) << " = Default::default();\n";

    size_t const n_required = s.fields.size() - n_optional;

    if (n_required > 0)
	for (size_t ii = 0; ii <= n_required / 32; ++ii) {
	    size_t const bits =
		32 - std::min(size_t(n_required - ii * 32),
			      size_t(32));

	    os <<
		"\t      let mut req_fld" << ii << ": u32 = 0x" <<
		std::hex << (0xffffffffu >> bits) << std::dec <<
		"u32;\n";
	}

    os <<
	"\n"
	"\t      for _ in 0..(n / 2) {\n"
	"\t\tif let Some(fld) = <i16 as Xlat>::from_iter(iter) {\n"
	"\t\t  match *fld {\n";

    size_t req_count = 0;

    for (FieldList::const_iterator jj = s.fields.begin();
	 jj != s.fields.end(); ++jj) {
	os <<
	    "\t\t    " << jj->hash << " => \n"
	    "\t\t      if let Some(v) = <" <<
	    type_name(p, *jj, true) << " as Xlat>::from_iter(iter) {\n"
	    "\t\t\ttmp_" << toSnake(jj->name) <<
	    " = " << (jj->optional ? "Some(*v)" : "*v") << ";\n";
	if (!jj->optional) {
	    os <<
		"\t\t\t  req_fld" << (req_count / 32) <<
		" &= !0x" << std::hex << (1u << (req_count % 32)) <<
		std::dec << "\n";
	    ++req_count;
	}

	os <<
	    "\t\t      } else {\n"
	    "\t\t\treturn None\n"
	    "\t\t      },\n";
    }

    os <<
	"\t\t    _ =>\n"
	"\t\t      return None\n"
	"\t\t  }\n"
	"\t\t} else {\n"
	"\t\t  return None\n"
	"\t\t}\n"
	"\t      }\n"
	"\n";

    if (n_required > 0) {
	os <<
	    "\t      // Make sure all the required fields have been\n"
	    "\t      // provided by the iterator.\n\n";

	for (size_t ii = 0; ii <= n_required / 32; ++ii)
	    os <<
		(ii == 0 ? "\t      if " : " ||\n\t\t ") <<
		"req_fld" << ii << " != 0";

	os <<
	    " {\n"
	    "\t\treturn None\n"
	    "\t      }\n\n";
    }

    os <<
	"\t      Some(" << tn << "::" << s.name <<
	" { ";

    std::string const indent =
	std::string(24 + tn.length() + name.length(), ' ');

    for (FieldList::const_iterator jj = s.fields.begin();
	 jj != s.fields.end(); ++jj) {
	std::string const fldName = toSnake(jj->name);

	if (jj != s.fields.begin())
	    os << ",\n" << indent;
	os <<
	    fldName << ": tmp_" << fldName;
    }

    os <<
	" })\n"
	"\t    }\n"
	"\t    _ => None\n"
	"\t  }\n"
	"\t},\n";
}

static void dumpMessage(Struct::Usage const& kind, std::ostream& os,
			Protocol const& p)
{
    StructList const& types =
	kind == Struct::asRequest ? p.requests : p.replies;
    std::string const tn = (kind == Struct::asRequest ? "Request" : "Reply");

    os <<
	"#[derive(Debug)]\n"
	"pub enum " << tn << " {\n";

    for (StructList::const_iterator ii = types.begin(); ii != types.end();
	 ++ii) {
	std::string const name = toTypeName(ii->name);

	os <<
	    "  " << name;

	if (!ii->fields.empty()) {
	    os << " { ";

	    std::string const indent(5 + name.length(), ' ');

	    for (FieldList::const_iterator jj = ii->fields.begin();
		 jj != ii->fields.end(); ++jj) {
		if (jj != ii->fields.begin())
		    os << ",\n" << indent;
		os << toSnake(jj->name) << ": " <<
		    type_name(p, *jj);
	    }
	    os << " }";
	}
	os << ",\n";
    }

    os <<
	"}\n\n"
	"impl" << " " << tn << " {\n"
	"  #[allow(dead_code)]\n"
	"  pub fn marshal(&self) -> Vec<u8> {\n"
	"    let mut buf = vec!['S' as u8, 'D' as u8, 'D' as u8, 0x02u8,\n"
	"\t\t       0x51u8, 0x03u8];\n\n"
	"    Xlat::to_buf(&" << p.longHash(p.name) << "i32, &mut buf);\n"
	"    match self {\n";

    for (StructList::const_iterator ii = types.begin(); ii != types.end();
	 ++ii) {
	size_t n_optional = 0;

	os <<
	    "      " << tn << "::" << toTypeName(ii->name);

	if (!ii->fields.empty()) {
	    os << " { ";

	    for (FieldList::const_iterator jj = ii->fields.begin();
		 jj != ii->fields.end(); ++jj) {
		if (jj != ii->fields.begin())
		    os << ", ";
		os << toSnake(jj->name);
		if (jj->optional)
		    ++n_optional;
	    }
	    os << " }";
	}

	os <<
	    " => {\n"
	    "\tlet n_fields = " << (ii->fields.size() * 2);

	for (FieldList::const_iterator jj = ii->fields.begin();
	     jj != ii->fields.end(); ++jj)
	    if (jj->optional)
		os <<
		    " -\n\t\t       "
		    "(if *" << toSnake(jj->name) << " == None { 2 } else { 0 })";

	os <<
	    ";\n\n"
	    "\t<i16 as Xlat>::to_buf(&" << ii->hash << ", &mut buf);\n"
	    "\tint_to_buf(0x50u8, n_fields, &mut buf);\n";

	for (FieldList::const_iterator jj = ii->fields.begin();
	     jj != ii->fields.end(); ++jj) {
	    std::string const fldName = toSnake(jj->name);

	    if (jj->optional)
		os <<
		    "\tif let Some(v) = " << fldName << " {\n"
		    "\t  int_to_buf(0x10u8, " << jj->hash << ", &mut buf);\n"
		    "\t  Xlat::to_buf(v, &mut buf);\n"
		    "\t}\n";
	    else
		os <<
		    "\tint_to_buf(0x10u8, " << jj->hash << ", &mut buf);\n"
		    "\tXlat::to_buf(" << toSnake(jj->name) << ", &mut buf);\n";
	}

	os << "      }\n";
    }

    os <<
	"    };\n"
	"    buf\n"
	"  }\n\n"
	"  #[allow(dead_code)]\n"
	"  pub fn unmarshal(iter: &mut Iterator<Item=u8>) -> Option<" << tn <<
	"> {\n"
	"    if iter.next() != Some('S' as u8) || iter.next() != Some('D' as u8) ||\n"
	"       iter.next() != Some('D' as u8) || iter.next() != Some(0x02u8) ||\n"
	"       get_length(0x50, iter) != Some(3) ||\n"
	"       get_int(0x10, iter) != Some(" << p.longHash(p.name) << ") {\n"
	"      return None\n"
	"    }\n"
	"\n"
	"    if let Some(msg) = get_int(0x10, iter) {\n"
	"      match msg as i16 {\n";

    for (StructList::const_iterator ii = types.begin(); ii != types.end();
	 ++ii)
	dumpSingleMessage(tn, os, p, *ii);

    os <<
	"\t_ => None\n"
	"      }\n"
	"    } else {\n"
	"      None\n"
	"    }\n"
	"  }\n"
	"}\n\n";
}

static void dumpSourceFile(std::ostream& os, Protocol const& p)
{
    os <<
	"#[allow(dead_code)]\n" <<
	rustAllPublic <<
	"fn int_to_buf(tag: u8, v: i64, buf: &mut Vec<u8>) -> ()\n"
	"{\n"
	"  let b = v.to_be_bytes();\n"
	"\n"
	"  if v < -0x80 || v > 0x7f {\n"
	"    if v < -0x8000 || v > 0x7fff {\n"
	"      if v < -0x80_0000 || v > 0x7f_ffff {\n"
	"\tif v < -0x8000_0000 || v > 0x7fff_ffff {\n"
	"\t  if v < -0x80_0000_0000 || v > 0x7f_ffff_ffff {\n"
	"\t    if v < -0x8000_0000_0000 || v > 0x7fff_ffff_ffff {\n"
	"\t      if v < -0x80_0000_0000_0000 || v > 0x7f_ffff_ffff_ffff {\n"
	"\t\tbuf.push(tag | 8u8);\n"
	"\t\tbuf.push(b[0]);\n"
	"\t      } else {\n"
	"\t\tbuf.push(tag | 7u8);\n"
	"\t      }\n"
	"\t      buf.push(b[1]);\n"
	"\t    } else {\n"
	"\t      buf.push(tag | 6u8);\n"
	"\t    }\n"
	"\t    buf.push(b[2]);\n"
	"\t  } else {\n"
	"\t    buf.push(tag | 5u8);\n"
	"\t  }\n"
	"\t  buf.push(b[3]);\n"
	"\t} else {\n"
	"\t  buf.push(tag | 4u8);\n"
	"\t}\n"
	"\tbuf.push(b[4]);\n"
	"      } else {\n"
	"\tbuf.push(tag | 3u8);\n"
	"      }\n"
	"      buf.push(b[5]);\n"
	"    } else {\n"
	"      buf.push(tag | 2u8);\n"
	"    }\n"
	"    buf.push(b[6]);\n"
	"  } else {\n"
	"    buf.push(tag | 1u8);\n"
	"  }\n"
	"  buf.push(b[7]);\n"
	"}\n"
	"\n"
	"#[allow(dead_code)]\n" <<
	rustAllPublic <<
	"fn len_to_buf(tag: u8, v: u32, buf: &mut Vec<u8>) -> ()\n"
	"{\n"
	"  let b = v.to_be_bytes();\n"
	"\n"
	"  if v > 0x7f {\n"
	"    if v > 0x7fff {\n"
	"      if v > 0x7f_ffff {\n"
	"\tbuf.push(tag | 4u8);\n"
	"\tbuf.push(b[0]);\n"
	"      } else {\n"
	"\tbuf.push(tag | 3u8);\n"
	"      }\n"
	"      buf.push(b[1]);\n"
	"    } else {\n"
	"      buf.push(tag | 2u8);\n"
	"    }\n"
	"    buf.push(b[2]);\n"
	"  } else {\n"
	"    buf.push(tag | 1u8);\n"
	"  }\n"
	"  buf.push(b[3]);\n"
	"}\n"
	"\n"
	"#[allow(dead_code)]\n" <<
	rustAllPublic <<
	"fn get_int(tag: u8, iter: &mut Iterator<Item = u8>) -> Option<i64> {\n"
	"  if let Some(t) = iter.next() {\n"
	"    if (t & 0xf0) == tag {\n"
	"      return match t & 0x0f {\n"
	"\tn if n <= 8 => {\n"
	"\t  let mut v: i64 = 0;\n"
	"\n"
	"\t  for _ in 0..n {\n"
	"\t    match iter.next() {\n"
	"\t      Some(x) => v = v.wrapping_shl(8) | (x as i64),\n"
	"\t      None => return None,\n"
	"\t    }\n"
	"\t  }\n"
	"\n"
	"\t  let adj: u32 = (8 - (n as u32)) * 8;\n"
	"\n"
	"\t  Some(v.wrapping_shl(adj) >> adj)\n"
	"\t}\n"
	"\t_ => None,\n"
	"      };\n"
	"    }\n"
	"  }\n"
	"  return None;\n"
	"}\n"
	"\n"
	"#[allow(dead_code)]\n" <<
	rustAllPublic <<
	"fn get_length(tag: u8, iter: &mut Iterator<Item=u8>) -> Option<usize>\n"
	"{\n"
	"    match get_int(tag, iter) {\n"
	"\tSome(v) if v >= 0 && v <= 0x7fffffff => Some(v as usize),\n"
	"\t_ => None\n"
	"    }\n"
	"}\n"
	"\n" <<
	rustAllPublic <<
	"trait Xlat {\n"
	"  fn to_buf(&self, buf: &mut Vec<u8>) -> ();\n"
	"  fn from_iter(iter: &mut Iterator<Item=u8>) -> Option<Box<Self>>;\n"
	"}\n"
	"\n"
	"impl Xlat for bool {\n"
	"  #[allow(dead_code)]\n"
	"  fn to_buf(&self, buf: &mut Vec<u8>) -> () {\n"
	"    match self {\n"
	"      true => buf.push(0x71u8),\n"
	"      false => buf.push(0x70u8)\n"
	"    }\n"
	"  }\n\n"
	"  #[allow(dead_code)]\n"
	"  fn from_iter(iter: &mut Iterator<Item=u8>) -> Option<Box<bool>> {\n"
	"    match iter.next() {\n"
	"      Some(0x71u8) => Some(Box::new(true)),\n"
	"      Some(0x70u8) => Some(Box::new(false)),\n"
	"      _ => None"
	"    }\n"
	"  }\n"
	"}\n"
	"\n"
	"impl Xlat for i16 {\n"
	"  #[allow(dead_code)]\n"
	"  fn to_buf(&self, buf: &mut Vec<u8>) -> () {\n"
	"    int_to_buf(0x10u8, self.clone() as i64, buf);\n"
	"  }\n\n"
	"  #[allow(dead_code)]\n"
	"  fn from_iter(iter: &mut Iterator<Item=u8>) -> Option<Box<i16>> {\n"
	"    match get_int(0x10, iter) {\n"
	"      Some(v) if v >= -0x8000 && v <= 0x7fff =>\n"
	"\t  Some(Box::new(v as i16)),\n"
	"      _ => None\n"
	"    }\n"
	"  }\n"
	"}\n"
	"\n"
	"impl Xlat for i32 {\n"
	"  #[allow(dead_code)]\n"
	"  fn to_buf(&self, buf: &mut Vec<u8>) -> () {\n"
	"    int_to_buf(0x10u8, self.clone() as i64, buf);\n"
	"  }\n\n"
	"  #[allow(dead_code)]\n"
	"  fn from_iter(iter: &mut Iterator<Item=u8>) -> Option<Box<i32>> {\n"
	"    match get_int(0x10, iter) {\n"
	"      Some(v) if v >= -0x80000000 && v <= 0x7fffffff =>\n"
	"\t  Some(Box::new(v as i32)),\n"
	"      _ => None\n"
	"    }\n"
	"  }\n"
	"}\n"
	"\n"
	"impl Xlat for i64 {\n"
	"  #[allow(dead_code)]\n"
	"  fn to_buf(&self, buf: &mut Vec<u8>) -> () {\n"
	"    int_to_buf(0x10u8, self.clone() as i64, buf);\n"
	"  }\n\n"
	"  #[allow(dead_code)]\n"
	"  fn from_iter(iter: &mut Iterator<Item=u8>) -> Option<Box<i64>> {\n"
	"    match get_int(0x10, iter) {\n"
	"      Some(v) => Some(Box::new(v)),\n"
	"      None => None\n"
	"    }\n"
	"  }\n"
	"}\n"
	"\n"
	"impl Xlat for f64 {\n"
	"  #[allow(dead_code)]\n"
	"  fn to_buf(&self, buf: &mut Vec<u8>) -> () {\n"
	"    let raw = self.to_bits().to_be_bytes();\n"
	"\n"
	"    buf.push(0x28u8);\n"
	"    buf.extend_from_slice(&raw);\n"
	"  }\n\n"
	"  #[allow(dead_code)]\n"
	"  fn from_iter(iter: &mut Iterator<Item=u8>) -> Option<Box<f64>> {\n"
	"    match get_int(0x20, iter) {\n"
	"      Some(v) => Some(Box::new(f64::from_bits(v as u64))),\n"
	"      _ => None\n"
	"    }\n"
	"  }\n"
	"}\n"
	"\n"
	"impl Xlat for String {\n"
	"  #[allow(dead_code)]\n"
	"  fn to_buf(&self, buf: &mut Vec<u8>) -> () {\n"
	"    buf.reserve(5 + self.len());\n"
	"    len_to_buf(0x40u8, self.len() as u32, buf);\n"
	"    buf.extend_from_slice(self.as_bytes());\n"
	"  }\n\n"
	"  #[allow(dead_code)]\n"
	"  fn from_iter(iter: &mut Iterator<Item=u8>) -> Option<Box<String>> {\n"
	"    match get_length(0x40, iter) {\n"
	"      Some(n) => {\n"
	"\tlet mut tmp: Vec<u8> = Vec::with_capacity(n);\n\n"
	"\tfor _ in 0..n {\n"
	"\t  if let Some(ch) = iter.next() {\n"
	"\t    tmp.push(ch);\n"
	"\t  } else {\n"
	"\t    return None;\n"
	"\t  }\n"
	"\t};\n"
	"\tSome(Box::new(String::from_utf8_lossy(&tmp).into_owned()))\n"
	"      }\n"
	"      _ => None\n"
	"    }\n"
	"  }\n"
	"}\n"
	"\n"
	"impl Xlat for Vec<u8> {\n"
	"  #[allow(dead_code)]\n"
	"  fn to_buf(&self, buf: &mut Vec<u8>) -> () {\n"
	"    buf.reserve(5 + self.len());\n"
	"    len_to_buf(0x30u8, self.len() as u32, buf);\n"
	"    buf.extend_from_slice(&self);\n"
	"  }\n\n"
	"  #[allow(dead_code)]\n"
	"  fn from_iter(iter: &mut Iterator<Item=u8>) -> Option<Box<Vec<u8>>> {\n"
	"    match get_length(0x30, iter) {\n"
	"      Some(n) => {\n"
	"\tlet mut tmp: Vec<u8> = Vec::with_capacity(n);\n\n"
	"\tfor _ in 0..n {\n"
	"\t  if let Some(ch) = iter.next() {\n"
	"\t    tmp.push(ch);\n"
	"\t  } else {\n"
	"\t    return None;\n"
	"\t  }\n"
	"\t};\n"
	"\tSome(Box::new(tmp))\n"
	"      }\n"
	"      _ => None\n"
	"    }\n"
	"  }\n"
	"}\n"
	"\n"
	"impl<T> Xlat for Vec<T> where T: Xlat {\n"
	"  #[allow(dead_code)]\n"
	"  fn to_buf(&self, buf: &mut Vec<u8>) -> () {\n"
	"    len_to_buf(0x50u8, self.len() as u32, buf);\n"
	"    for ii in self {\n"
	"      Xlat::to_buf(ii, buf);\n"
	"    }\n"
	"  }\n\n"
	"  #[allow(dead_code)]\n"
	"  fn from_iter(iter: &mut Iterator<Item=u8>) -> Option<Box<Vec<T>>> {\n"
	"    match get_length(0x50, iter) {\n"
	"      Some(n) => {\n"
	"\tlet mut tmp = Vec::with_capacity(n);\n\n"
	"\tfor _ in 0..n {\n"
	"	   match Xlat::from_iter(iter) {\n"
	"\t    Some(v) => tmp.push(*v),\n"
	"\t    None => return None\n"
	"\t  }\n"
	"\t};\n"
	"\tSome(Box::new(tmp))\n"
	"      }\n"
	"      _ => None\n"
	"    }\n"
	"  }\n"
	"}\n\n";

    dumpEnums(os, p);
    dumpStructs(os, p);
    dumpMessage(Struct::asRequest, os, p);
    dumpMessage(Struct::asReply, os, p);
}

void generateRust(Protocol const& p)
{
    if (sddVersion != 2)
	throw std::runtime_error("Rust generator only supports SDDv2");

    std::string const srcName =
	targetPath + "/" + toSnake(p.name) + "_protocol.rs";
    std::ofstream src(srcName.c_str());

    dumpSourceFile(src, p);
}
