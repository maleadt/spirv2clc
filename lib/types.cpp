std::string translator::src_pointer_type(uint32_t storage, uint32_t tyid, bool signedty) const {
  std::string typestr;
  if (type_for(tyid)->kind() == Type::Kind::kArray) {
    auto tarray = type_for(tyid)->AsArray();
    auto elemty = tarray->element_type();
    typestr += src_type(type_id_for(elemty));
  } else {
    if (signedty) {
        typestr += src_type_signed(tyid);
    } else {
        typestr += src_type(tyid);
    }
  }
  typestr += " ";
  switch (storage) {
  case SpvStorageClassCrossWorkgroup:
    typestr += "global";
    break;
  case SpvStorageClassUniformConstant:
    typestr += "constant";
    break;
  case SpvStorageClassWorkgroup:
    typestr += "local";
    break;
  case SpvStorageClassInput:
  case SpvStorageClassFunction:
    break;
  default:
    std::cerr << "UNIMPLEMENTED pointer storage class " << storage
              << std::endl;
    return "UNIMPLEMENTED";
  }

  typestr += "*";
  return typestr;
}

bool translator::translate_type(const Instruction &inst) {
  std::string typestr;
  std::string signedtypestr;
  auto opcode = inst.opcode();
  auto result = inst.result_id();
  switch (opcode) {
  case spv::Op::OpTypePointer: {
    auto storage = inst.GetSingleWordOperand(1);
    auto type = inst.GetSingleWordOperand(2);
    if (m_types_signed.count(type)) {
      signedtypestr = src_pointer_type(storage, type, true);
    }
    typestr = src_pointer_type(storage, type, false);
    break;
  }
  case spv::Op::OpTypeInt: {
    auto width = inst.GetSingleWordOperand(1);
    switch (width) {
    case 8:
      typestr = "uchar";
      signedtypestr = "char";
      break;
    case 16:
      typestr = "ushort";
      signedtypestr = "short";
      break;
    case 32:
      typestr = "uint";
      signedtypestr = "int";
      break;
    case 64:
      typestr = "ulong";
      signedtypestr = "long";
      break;
    default:
      std::cerr << "UNIMPLEMENTED OpTypeInt width " << width << std::endl;
      return false;
    }
    break;
  }
  case spv::Op::OpTypeFloat: {
    auto width = inst.GetSingleWordOperand(1);
    switch (width) {
    case 16:
      typestr = "half";
      break;
    case 32:
      typestr = "float";
      break;
    case 64:
      typestr = "double";
      break;
    default:
      std::cerr << "UNIMPLEMENTED OpTypeFloat width " << width << std::endl;
      return false;
    }
    break;
  }
  case spv::Op::OpTypeVector: {
    auto ctype = inst.GetSingleWordOperand(1);
    auto cnum = inst.GetSingleWordOperand(2);
    typestr = src_type(ctype) + std::to_string(cnum);
    signedtypestr = src_type_signed(ctype) + std::to_string(cnum);
    break;
  }
  case spv::Op::OpTypeStruct: { // TODO support volatile members
    // Declare the structure type
    m_src << "struct " + var_for(result) + " {" << std::endl;
    for (uint32_t opidx = 1; opidx < inst.NumOperands(); opidx++) {
      auto mid = inst.GetSingleWordOperand(opidx);
      // Convert pointer members to integers, as most OpenCL compilers refuse
      // structures with pointer members (even though OpenCL 2.0 allows it).
      auto member_type = type_for(mid);
      if (member_type->kind() == Type::Kind::kPointer) {
        m_src << "  ulong m" << std::to_string(opidx - 1) << ";" << std::endl;
      } else {
        m_src << "  " << src_var_decl(mid, "m" + std::to_string(opidx - 1)) << ";"
              << std::endl;
      }
    }
    m_src << "}";
    if (m_packed.count(result)) {
      m_src << " __attribute__((packed))";
    }
    m_src << ";" << std::endl;

    // Prepare the type name
    typestr = "struct " + var_for(result);
    break;
  }
  case spv::Op::OpTypeArray: {
    // Array types are handled in variable declarations
    // We don't register a type name for them since they need special syntax
    break;
  }
  case spv::Op::OpTypeImage: {
    // auto sampledty = inst.GetSingleWordOperand(1);
    auto dim = inst.GetSingleWordOperand(2);
    auto depth = inst.GetSingleWordOperand(3);
    auto arrayed = inst.GetSingleWordOperand(4);
    auto ms = inst.GetSingleWordOperand(5);
    auto sampled = inst.GetSingleWordOperand(6);
    // auto format = inst.GetSingleWordOperand(7);
    auto qual = inst.GetSingleWordOperand(8);

    if ((depth != 0) || (arrayed != 0) || (ms != 0) || (sampled != 0)) {
      std::cerr << "UNIMPLEMENTED image type (depth = " << depth
                << ", arrayed = " << arrayed << ", ms = " << ms
                << "sampled = " << sampled << ")" << std::endl;
      return false;
    }

    switch (qual) {
    case SpvAccessQualifierReadOnly:
      typestr = "read_only";
      break;
    case SpvAccessQualifierWriteOnly:
      typestr = "write_only";
      break;
    case SpvAccessQualifierReadWrite:
      typestr = "read_write";
      break;
    default:
      std::cerr << "UNIMPLEMENTED image access qualifier " << qual << std::endl;
      return false;
    }

    typestr += " ";

    switch (dim) {
    case SpvDim1D:
      typestr += "image1d_t";
      break;
    case SpvDim2D:
      typestr += "image2d_t";
      break;
    case SpvDim3D:
      typestr += "image3d_t";
      break;
    default:
      std::cerr << "UNIMPLEMENTED image dimensionality " << dim << std::endl;
      return false;
    }

    break;
  }
  case spv::Op::OpTypeSampledImage: // TODO anything?
    break;
  case spv::Op::OpTypeSampler:
    typestr = "sampler_t";
    break;
  case spv::Op::OpTypeOpaque: {
    auto name = inst.GetOperand(1).AsString();
    typestr = "struct " + name;
    m_src << typestr << ";" << std::endl;
    break;
  }
  case spv::Op::OpTypeBool:
    typestr = "bool";
    break;
  case spv::Op::OpTypeVoid:
    typestr = "void";
    break;
  case spv::Op::OpTypeFunction: // FIXME
    break;
  case spv::Op::OpTypeEvent:
    typestr = "event_t";
    break;
  default:
    std::cerr << "UNIMPLEMENTED type instuction " << opcode << std::endl;
    return false;
  }

  m_types[result] = typestr;
  if (signedtypestr != "") {
    m_types_signed[result] = signedtypestr;
  }

  return true;
}

bool translator::translate_types_values() {
  for (auto &inst : m_ir->module()->types_values()) {
    auto opcode = inst.opcode();
    auto rtype = inst.type_id();
    auto result = inst.result_id();

    switch (opcode) {
    case spv::Op::OpTypeInt:
    case spv::Op::OpTypeVector:
    case spv::Op::OpTypePointer:
    case spv::Op::OpTypeVoid:
    case spv::Op::OpTypeBool:
    case spv::Op::OpTypeFunction:
    case spv::Op::OpTypeFloat:
    case spv::Op::OpTypeStruct:
    case spv::Op::OpTypeArray:
    case spv::Op::OpTypeOpaque:
    case spv::Op::OpTypeImage:
    case spv::Op::OpTypeSampler:
    case spv::Op::OpTypeSampledImage:
    case spv::Op::OpTypeEvent:
      if (!translate_type(inst)) {
        return false;
      }
      break;

    case spv::Op::OpConstant: {
      auto &op_val = inst.GetOperand(2);
      auto type = type_for(rtype);
      switch (type->kind()) {
      case Type::Kind::kInteger: {
        auto tint = type->AsInteger();
        if (tint->width() <= 32) {
          m_literals[result] = src_cast(rtype, std::to_string(op_val.words[0]));
        } else if (tint->width() == 64) {
          uint64_t w0 = op_val.words[0];
          uint64_t w1 = op_val.words[1];
          auto w = w1 << 32 | w0;
          m_literals[result] = src_cast(rtype, std::to_string(w));
        } else {
          std::cerr << "UNIMPLEMENTED integer constant width " << tint->width()
                    << std::endl;
          return false;
        }
        break;
      }
      case Type::Kind::kFloat: {
        auto tfloat = type->AsFloat();
        auto width = tfloat->width();
        std::ostringstream out;
        if (width == 16) {
          uint32_t w0 = op_val.words[0];
          cl_half h = w0 & 0xFFFF;
          float val = cl_half_to_float(h);
          out.precision(11);
          out << std::fixed << val << "h";
        } else if (width == 32) {
          uint32_t w0 = op_val.words[0];
          float val = *reinterpret_cast<float *>(&w0);
          if (std::isinf(val)) {
            if (std::signbit(val)) {
              out << "-";
            }
            out << "INFINITY";
          } else if (std::isnan(val)) {
            out << "NAN";
          } else {
            out.precision(24);
            out << std::fixed << val << "f";
          }
        } else if (width == 64) {
          uint64_t w0 = op_val.words[0];
          uint64_t w1 = op_val.words[1];
          auto w = w1 << 32 | w0;
          double val = *reinterpret_cast<double *>(&w);
          if (std::isinf(val)) {
            if (std::signbit(val)) {
              out << "-";
            }
            out << "INFINITY";
          } else if (std::isnan(val)) {
            out << "NAN";
          } else {
            out.precision(53);
            out << std::fixed << val;
          }
        } else {
          std::cerr << "UNIMPLEMENTED float constant width " << width
                    << std::endl;
          return false;
        }
        m_literals[result] = out.str();
        break;
      }
      default:
        std::cerr << "UNIMPLEMENTED OpConstant type " << type->kind()
                  << std::endl;
        return false;
      }
      break;
    }
    case spv::Op::OpUndef:
    case spv::Op::OpConstantNull: {
      std::string cst;
      if (!get_null_constant(rtype, cst)) {
        return false;
      }
      m_literals[result] = cst;
      break;
    }
    case spv::Op::OpConstantTrue: {
      m_literals[result] = "true";
      break;
    }
    case spv::Op::OpConstantFalse: {
      m_literals[result] = "false";
      break;
    }
    case spv::Op::OpConstantSampler: {
      auto addressing_mode = inst.GetSingleWordOperand(2);
      auto normalised = inst.GetSingleWordOperand(3);
      auto filter_mode = inst.GetSingleWordOperand(4);
      m_src << "constant sampler_t " << var_for(result) << " = ";
      switch (addressing_mode) {
      case SpvSamplerAddressingModeClampToEdge:
        m_src << "CLK_ADDRESS_CLAMP_TO_EDGE";
        break;
      case SpvSamplerAddressingModeClamp:
        m_src << "CLK_ADDRESS_CLAMP";
        break;
      case SpvSamplerAddressingModeRepeat:
        m_src << "CLK_ADDRESS_REPEAT";
        break;
      case SpvSamplerAddressingModeRepeatMirrored:
        m_src << "CLK_ADDRESS_MIRRORED_REPEAT";
        break;
      case SpvSamplerAddressingModeNone:
        m_src << "CLK_ADDRESS_NONE";
        break;
      }

      m_src << " | ";

      if (normalised) {
        m_src << "CLK_NORMALIZED_COORDS_TRUE";
      } else {
        m_src << "CLK_NORMALIZED_COORDS_FALSE";
      }

      m_src << " | ";

      switch (filter_mode) {
      case SpvSamplerFilterModeNearest:
        m_src << "CLK_FILTER_NEAREST";
        break;
      case SpvSamplerFilterModeLinear:
        m_src << "CLK_FILTER_LINEAR";
        break;
      }

      m_src << ";" << std::endl;

      break;
    }
    case spv::Op::OpConstantComposite: {
      auto type = type_for(rtype);
      std::string lit;
      switch (type->kind()) {
      case Type::Kind::kVector: {
        auto tvec = type->AsVector();
        // ((type)(c0, c1, ..., cN))
        lit = "((" + src_type(rtype) + ")(";
        const char *sep = "";
        for (uint32_t opidx = 2; opidx < tvec->element_count() + 2; opidx++) {
          auto cid = inst.GetSingleWordOperand(opidx);
          lit += sep;
          lit += m_literals[cid];
          sep = ", ";
        }
        lit += "))";
        m_literals[result] = lit;
        break;
      }
      case Type::Kind::kStruct: {
        auto tstruct = type->AsStruct();
        // ((type){m0, m1, ..., mN})
        lit = "((" + src_type(rtype) + "){";
        const char *sep = "";
        for (uint32_t opidx = 2; opidx < tstruct->element_types().size() + 2;
             opidx++) {
          auto mid = inst.GetSingleWordOperand(opidx);
          lit += sep;
          lit += m_literals[mid];
          sep = ", ";
        }
        lit += "})";
        m_literals[result] = lit;
        break;
      }
      case Type::Kind::kArray: {
        lit = "{";
        const char *sep = "";
        uint32_t num_elems = array_type_get_length(rtype);
        if (num_elems == 0) {
            return false;
        }

        for (uint32_t opidx = 2; opidx < num_elems + 2; opidx++) {
          auto mid = inst.GetSingleWordOperand(opidx);
          lit += sep;
          lit += m_literals[mid];
          sep = ", ";
        }
        lit += "}";
        m_literals[result] = lit;
        break;
      }
      default:
        std::cerr << "UNIMPLEMENTED OpConstantComposite type " << type->kind()
                  << std::endl;
        return false;
      }
      break;
    }
    case spv::Op::OpVariable: {
      if (m_builtin_variables.count(result) != 0) {
        break;
      }

      auto tyvar = type_for(rtype);
      auto tykind = tyvar->kind();
      if (tykind != Type::Kind::kPointer) {
        std::cerr << "UNIMPLEMENTED global variable with type " << tykind
                  << std::endl;
        return false;
      }

      auto typtr = tyvar->AsPointer();
      auto tymgr = m_ir->get_type_mgr();
      auto typointeeid = tymgr->GetId(typtr->pointee_type());

      auto storage = inst.GetSingleWordOperand(2);

      if (storage == SpvStorageClassWorkgroup) {
        std::string local_var_decl = "local " + src_type_memory_object_declaration(typointeeid, result);
        m_local_variable_decls[result] = local_var_decl;
      } else if (storage == SpvStorageClassUniformConstant) {
        // Check if initializer is a string array and cache it for later use
        if (inst.NumOperands() > 3) {
          auto init = inst.GetSingleWordOperand(3);
          auto defuse = m_ir->get_def_use_mgr();
          auto init_inst = defuse->GetDef(init);
          if (init_inst &&
              init_inst->opcode() == spv::Op::OpConstantComposite) {
            auto string_literal = get_string_literal(*init_inst);
            if (string_literal) {
              m_constant_string_literals[result] = *string_literal;
            }
          }
        }

        m_src << "constant "
              << src_type_memory_object_declaration(typointeeid, result);
        if (inst.NumOperands() > 3) {
          auto init = inst.GetSingleWordOperand(3);
          m_src << " = " << var_for(init);
        }
        m_src << ";" << std::endl;
      } else {
        std::cerr << "UNIMPLEMENTED global variable with storage class "
                  << storage << std::endl;
        return false;
      }

      break;
    }
    default:
      std::cerr << "UNIMPLEMENTED type/value instruction " << opcode << ".\n";
      return false;
    }
  }
  return true;
}
