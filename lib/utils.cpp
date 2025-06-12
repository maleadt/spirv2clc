uint32_t translator::type_id_for(uint32_t val) const {
  auto defuse = m_ir->get_def_use_mgr();
  return defuse->GetDef(val)->type_id();
}

uint32_t
translator::type_id_for(const spvtools::opt::analysis::Type *type) const {
  return m_ir->get_type_mgr()->GetId(type);
}

spvtools::opt::analysis::Type *translator::type_for(uint32_t tyid) const {
  return m_ir->get_type_mgr()->GetType(tyid);
}

spvtools::opt::analysis::Type *translator::type_for_val(uint32_t val) const {
  return type_for(type_id_for(val));
}

uint32_t translator::array_type_get_length(uint32_t tyid) const {
  auto type = type_for(tyid);
  auto tarray = type->AsArray();
  auto const &length_info = tarray->length_info();
  if (length_info.words[0] !=
      spvtools::opt::analysis::Array::LengthInfo::kConstant) {
    std::cerr << "UNIMPLEMENTED array type with non-constant length"
              << std::endl;
    return 0;
  }

  if (length_info.words.size() > 2) {
    for (unsigned i = 2; i < length_info.words.size(); i++) {
      if (length_info.words[i] != 0) {
        std::cerr << "UNIMPLEMENTED array type with huge size" << std::endl;
        return 0;
      }
    }
  }

  return length_info.words[1];
}

std::string translator::src_var_decl(uint32_t tyid, const std::string &name,
                                     uint32_t val) const {
  auto ty = type_for(tyid);

  // Arrays (and pointers to arrays) need special handling; they don't have
  // a valid type name registered because they require special syntax.
  if (ty->kind() == spvtools::opt::analysis::Type::Kind::kArray) {
    // Collect all array dimensions by walking through nested arrays
    std::vector<uint32_t> dimensions;
    const auto* current_ty = ty;

    while (current_ty->kind() == spvtools::opt::analysis::Type::Kind::kArray) {
      auto aty = current_ty->AsArray();
      auto cstmgr = m_ir->get_constant_mgr();
      auto ecnt = cstmgr->FindDeclaredConstant(aty->LengthId())->GetSignExtendedValue();
      dimensions.push_back(ecnt);
      current_ty = aty->element_type();
    }

    // Get the base element type (non-array)
    auto base_type_id = type_id_for(current_ty);
    std::string base_type;
    if (val != 0) {
      base_type = src_type_for_value(val);
    } else {
      base_type = src_type(base_type_id);
    }

    // Build the declaration: base_type name[dim1][dim2]...[dimN]
    std::string result = base_type + " " + name;
    for (uint32_t dim : dimensions) {
      result += "[" + std::to_string(dim) + "]";
    }
    return result;
  } else if (ty->kind() == spvtools::opt::analysis::Type::Kind::kPointer) {
    auto ptr_ty = ty->AsPointer();
    auto pointee_ty = ptr_ty->pointee_type();

    if (pointee_ty->kind() == spvtools::opt::analysis::Type::Kind::kArray) {
      // Collect all array dimensions by walking through nested arrays
      std::vector<uint32_t> dimensions;
      const auto *current_ty = pointee_ty;

      while (current_ty->kind() ==
             spvtools::opt::analysis::Type::Kind::kArray) {
        auto aty = current_ty->AsArray();
        auto cstmgr = m_ir->get_constant_mgr();
        auto ecnt = cstmgr->FindDeclaredConstant(aty->LengthId())
                        ->GetSignExtendedValue();
        dimensions.push_back(ecnt);
        current_ty = aty->element_type();
      }

      // Get the base pointer type (non-array)
      auto base_type_id = type_id_for(current_ty);
      auto storage = static_cast<uint32_t>(ptr_ty->storage_class());
      std::string base_with_storage =
          src_pointer_type(storage, base_type_id, false);

      // Remove the trailing "*" since we need special pointer-to-array syntax
      base_with_storage.pop_back();

      // Build the declaration:
      // base_type storage_qualifier (*name)[dim1][dim2]...[dimN]
      std::string result = base_with_storage + "(*" + name + ")";
      for (uint32_t dim : dimensions) {
        result += "[" + std::to_string(dim) + "]";
      }
      return result;
    }
  }

  if (val != 0) {
    return src_type_for_value(val) + " " + name;
  } else {
    return src_type(tyid) + " " + name;
  }
}

std::string
translator::src_access_chain(const std::string &src_base,
                             const spvtools::opt::analysis::Type *ty,
                             uint32_t index) const {
  std::string ret = "(" + src_base + ")";
  if (ty->kind() == spvtools::opt::analysis::Type::kStruct) {
    auto cstmgr = m_ir->get_constant_mgr();
    auto idxcst = cstmgr->FindDeclaredConstant(index);
    if (idxcst == nullptr) {
      return "UNIMPLEMENTED";
    }
    return "&(" + ret + "->m" + std::to_string(idxcst->GetZeroExtendedValue()) +
           ")";
  } else if (ty->kind() == spvtools::opt::analysis::Type::kArray) {
    return "&(" + ret + "[" + var_for(index) + "])";
  } else {
    return "UNIMPLEMENTED";
  }
}

std::string
translator::src_type_memory_object_declaration(uint32_t tid, uint32_t val,
                                               const std::string &name) const {
  std::string ret;
  if (type_for(tid)->kind() == Type::Kind::kArray) {
    auto tarray = type_for(tid)->AsArray();
    auto elemty = tarray->element_type();
    ret = src_type(type_id_for(elemty));
  } else {
    ret = src_type(tid);
  }
  if (m_restricts.count(val)) {
    ret += " restrict";
  }
  if (m_volatiles.count(val)) {
    ret += " volatile";
  }
  if (m_alignments.count(val)) {
    ret += " __attribute__((aligned(" + std::to_string(m_alignments.at(val)) +
           ")))";
  }
  ret += " " + name;
  if (type_for(tid)->kind() == Type::Kind::kArray) {
    auto len = array_type_get_length(tid);
    ret += "[" + std::to_string(len) + "]";
  }
  return ret;
}

std::string translator::src_type_boolean_for_val(uint32_t val) const {
  if (m_boolean_src_types.count(val)) {
    return m_boolean_src_types.at(val);
  } else {
    auto type = type_for_val(val);
    if (type->kind() != Type::Kind::kVector) {
      return "int";
    } else {
      auto vtype = type->AsVector();
      auto etype = vtype->element_type();
      auto ecnt = vtype->element_count();
      auto ekind = etype->kind();

      switch (ekind) {
      case Type::Kind::kInteger: {
        auto width = etype->AsInteger()->width();
        switch (width) {
        case 8:
          return "char" + std::to_string(ecnt);
        case 16:
          return "short" + std::to_string(ecnt);
        case 32:
          return "int" + std::to_string(ecnt);
        case 64:
          return "long" + std::to_string(ecnt);
        }
        break;
      }
      case Type::Kind::kFloat: {
        auto width = etype->AsFloat()->width();
        switch (width) {
        case 16:
          return "short" + std::to_string(ecnt);
        case 32:
          return "int" + std::to_string(ecnt);
        case 64:
          return "long" + std::to_string(ecnt);
        }
        break;
      }
      default:
        break;
      }
    }
  }

  std::cerr << "UNIMPLEMENTED type for translation to boolean" << std::endl;
  return "UNIMPLEMENTED TYPE FOR BOOLEAN";
}

bool translator::get_null_constant(uint32_t tyid, std::string &src) const {
  auto type = type_for(tyid);
  switch (type->kind()) {
  case Type::Kind::kInteger:
    src = src_cast(tyid, "0");
    break;
  case Type::Kind::kFloat:
    src = "0.0";
    break;
  case Type::Kind::kArray:
  case Type::Kind::kStruct:
    src = "{0}";
    break;
  case Type::Kind::kBool:
    src = "false";
    break;
  case Type::Kind::kVector:
    src = "((" + src_type(tyid) + ")(0))";
    break;
  case Type::Kind::kEvent:
    src = "0";
    break;
  default:
    std::cerr << "UNIMPLEMENTED null constant type " << type->kind()
              << std::endl;
    return false;
  }

  return true;
}

std::unordered_set<std::string> gReservedIdentifiers = {
    // ANSI / ISO C90
    "auto",
    "break",
    "case",
    "char",
    "const",
    "continue",
    "default",
    "do",
    "double",
    "else",
    "enum",
    "extern",
    "float",
    "for",
    "goto",
    "if",
    "int",
    "long",
    "register",
    "return",
    "short",
    "signed",
    "sizeof",
    "static",
    "struct",
    "switch",
    "typedef",
    "union",
    "unsigned",
    "void",
    "volatile",
    "while",
    // C99
    "_Bool",
    "_Complex",
    "_Imaginary",
    "inline",
    "restrict",
    // OpenCL C built-in vector data types
    "char2",
    "char3",
    "char4",
    "char8",
    "char16",
    "uchar2",
    "uchar3",
    "uchar4",
    "uchar8",
    "uchar16",
    "short2",
    "short3",
    "short4",
    "short8",
    "short16",
    "ushort2",
    "ushort3",
    "ushort4",
    "ushort8",
    "ushort16",
    "int2",
    "int3",
    "int4",
    "int8",
    "int16",
    "uint2",
    "uint3",
    "uint4",
    "uint8",
    "uint16",
    "long2",
    "long3",
    "long4",
    "long8",
    "long16",
    "ulong2",
    "ulong3",
    "ulong4",
    "ulong8",
    "ulong16",
    "float2",
    "float3",
    "float4",
    "float8",
    "float16",
    "double2",
    "double3",
    "double4",
    "double8",
    "double16",
    // OpenCL C other built-in data types
    "image2d_t",
    "image3d_t",
    "image2d_array_t",
    "image1d_t",
    "image1d_buffer_t",
    "image1d_array_t",
    "image2d_depth_t",
    "image2d_array_depth_t",
    "sampler_t",
    "queue_t",
    "ndrange_t",
    "clk_event_t",
    "reserve_id_t",
    "event_t",
    "clk_mem_fence_flags",
    // OpenCL C reserved data types
    "bool2",
    "bool3",
    "bool4",
    "bool8",
    "bool16",
    "half2",
    "half3",
    "half4",
    "half8",
    "half16",
    "quad",
    "quad2",
    "quad3",
    "quad4",
    "quad8",
    "quad16",
    "complex",
    "imaginary",
    // TODO {double,float}nxm
    // OpenCL address space qualifiers
    "__global",
    "global",
    "__local",
    "local",
    "__constant",
    "constant",
    "__private",
    "private",
    "__generic",
    "generic",
    // OpenCL C function qualifiers
    "__kernel",
    "kernel",
    // OpenCL C access qualifiers
    "__read_only",
    "read_only",
    "__write_only",
    "write_only",
    "__read_write",
    "read_write",
    // OpenCL C misc
    "uniform",
    "pipe",
};

bool translator::is_valid_identifier(const std::string& name) const {
  // Check the name isn't already used
  for (auto it = m_names.begin(); it != m_names.end(); ++it) {
    if (it->second == name) {
        return false;
    }
  }

  // Check the name is not a reserved identifier
  return gReservedIdentifiers.count(name) == 0;
}

std::string translator::make_valid_identifier(const std::string& name) const {
  std::string newname = name;

  bool is_valid = is_valid_identifier(newname);
  if (!is_valid) {
    newname += "_MADE_VALID_CLC_IDENT";
  }

  is_valid = is_valid_identifier(newname);

  int name_iter = 1;
  while(!is_valid) {
    std::string candidate = newname + std::to_string(name_iter);
    is_valid = is_valid_identifier(candidate);
    if (!is_valid) {
      name_iter++;
    } else {
      newname = candidate;
      break;
    }
  }

  return newname;
}

std::optional<std::string>
translator::get_string_literal(const spvtools::opt::Instruction &inst) const {
  auto rtype = inst.type_id();
  auto type = type_for(rtype);

  // Must be an array type
  if (type->kind() != Type::Kind::kArray) {
    return std::nullopt;
  }

  auto tarray = type->AsArray();
  auto elem_type = tarray->element_type();

  // Element type must be 8-bit integer (char/uchar)
  if (elem_type->kind() != Type::Kind::kInteger) {
    return std::nullopt;
  }

  auto tint = elem_type->AsInteger();
  if (tint->width() != 8) {
    return std::nullopt;
  }

  uint32_t num_elems = array_type_get_length(rtype);
  if (num_elems == 0) {
    return std::nullopt;
  }

  auto defuse = m_ir->get_def_use_mgr();

  // Check that the last element is a null terminator
  auto last_elem_id = inst.GetSingleWordOperand(num_elems - 1 + 2);
  auto last_elem_inst = defuse->GetDef(last_elem_id);
  if (!last_elem_inst || last_elem_inst->opcode() != spv::Op::OpConstant) {
    return std::nullopt;
  }
  auto &last_op_val = last_elem_inst->GetOperand(2);
  uint8_t last_value = static_cast<uint8_t>(last_op_val.words[0]);
  if (last_value != 0) {
    return std::nullopt;
  }

  std::string result = "\"";

  // Iterate through all elements except the last (null terminator)
  for (uint32_t i = 0; i < num_elems - 1; i++) {
    auto elem_id = inst.GetSingleWordOperand(i + 2);
    auto elem_inst = defuse->GetDef(elem_id);

    // Must be a constant
    if (!elem_inst || elem_inst->opcode() != spv::Op::OpConstant) {
      return std::nullopt;
    }

    // Get the raw constant value
    auto &op_val = elem_inst->GetOperand(2);
    uint8_t value = static_cast<uint8_t>(op_val.words[0]);

    // Bail out if we encounter a null within the string content
    if (value == 0) {
      return std::nullopt;
    }

    // Convert value to string literal character
    switch (value) {
    case 9:
      result += "\\t";
      break;
    case 10:
      result += "\\n";
      break;
    case 13:
      result += "\\r";
      break;
    case 34:
      result += "\\\"";
      break;
    case 92:
      result += "\\\\";
      break;
    default:
      if (value >= 32 && value <= 126) {
        result += static_cast<char>(value);
      } else {
        // Use octal escape for other values
        result += "\\";
        result += std::to_string((value >> 6) & 7);
        result += std::to_string((value >> 3) & 7);
        result += std::to_string(value & 7);
      }
      break;
    }
  }

  result += "\"";
  return result;
}

std::optional<std::string>
translator::string_literal_for(uint32_t var_id) const {
  auto it = m_constant_string_literals.find(var_id);
  if (it != m_constant_string_literals.end()) {
    return it->second;
  }
  return std::nullopt;
}
