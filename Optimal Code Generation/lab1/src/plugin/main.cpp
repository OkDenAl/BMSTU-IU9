#include <iostream>
#include <sstream>
#include <string>

#include <gcc-plugin.h>
#include <plugin-version.h>

#include <coretypes.h>

#include <tree-pass.h>
#include <context.h>
#include <basic-block.h>

#include <tree.h>
#include <tree-ssa-alias.h>
#include <gimple-expr.h>
#include <gimple.h>
#include <gimple-ssa.h>
#include <tree-phinodes.h>
#include <tree-ssa-operands.h>

#include <ssa-iterators.h>
#include <gimple-iterator.h>

int plugin_is_GPL_compatible = 1; // is licensed under a GPL-compatible license

void bb_info(basic_block bb) {
    std::cout << "\t" << "bb:\n";
    edge e;
    edge_iterator it;

    std::cout << "\t\tbefore: { ";
    std::stringstream src_stream;
    FOR_EACH_EDGE(e, it, bb->preds) {
        src_stream << e->src->index << ", ";
    }
    std::string src = src_stream.str();
    std::cout << src.substr(0, src.size() - 2);
    std::cout <<" }"<< std::endl;

    std::cout << "\t\tcurrent: { " << bb->index << " }\n";

    std::cout << "\t\tafter: { ";
    std::stringstream dst_stream;
    FOR_EACH_EDGE(e, it, bb->succs) {
        dst_stream << e->dest->index << ", ";
    }
    std::string dst = dst_stream.str();
    std::cout << dst.substr(0, dst.size() - 2);
    std::cout<<" }" << std::endl;
}

void get_tree(tree t) {
    switch (TREE_CODE(t)) {
    case INTEGER_CST:
        std::cout << "INTEGER_CST: "<< TREE_INT_CST_LOW(t);
        break;
    case STRING_CST:
        std::cout << "STRING_CST: " << TREE_STRING_POINTER(t);
        break;
    case LABEL_DECL: 
        std::cout << (DECL_NAME(t) ? IDENTIFIER_POINTER(DECL_NAME(t)) : "LABEL_DECL") << ":";
        break;
    case VAR_DECL:
        std::cout << (DECL_NAME(t) ? IDENTIFIER_POINTER(DECL_NAME(t)) : "VAR_DECL");
        break;
    case CONST_DECL:
        std::cout << (DECL_NAME(t) ? IDENTIFIER_POINTER(DECL_NAME(t)) : "CONST_DECL");
        break;
    case ARRAY_REF:
        std::cout << "ARRAY_REF ";
        get_tree(TREE_OPERAND(t, 0));
        std::cout << "[";
        get_tree(TREE_OPERAND(t, 1));
        std::cout << "]";
        break;
    
    case MEM_REF:
        std::cout << "MEM_REF ";
        std::cout << "((typeof(";
        get_tree(TREE_OPERAND(t, 1));
        std::cout << "))";
        get_tree(TREE_OPERAND(t, 0));
        std::cout << ")";
        break;
        
    case SSA_NAME: {
        gimple* st = SSA_NAME_DEF_STMT(t);
        if (gimple_code(st) == GIMPLE_PHI) {
            std::cout << "(" << (SSA_NAME_IDENTIFIER(t) ? IDENTIFIER_POINTER(SSA_NAME_IDENTIFIER(t)) : "SSA_NAME") <<
            "__v" << SSA_NAME_VERSION(t);
            std::cout << " = GIMPLE_PHI(";
            for (unsigned int i = 0; i < gimple_phi_num_args(st); i++) {
                get_tree(gimple_phi_arg(st, i)->def);
                if (i != gimple_phi_num_args(st) - 1) {
                    std::cout << ", ";
                }
            }
            std::cout << "))";
        } else {
            std::cout << (SSA_NAME_IDENTIFIER(t) ? IDENTIFIER_POINTER(SSA_NAME_IDENTIFIER(t)) : "SSA_NAME") <<
                "__v" << SSA_NAME_VERSION(t);
        }
        
        break;
    }
    default:
        std::cout << "UNDEFINED_TREE_CODE (" << TREE_CODE(t) << ")";
        break;
    }
}


void op(enum tree_code code) {
    switch (code) {
    case PLUS_EXPR:
        std::cout << "+";
        break;
    case MINUS_EXPR:
        std::cout << "-";
        break;
    case MULT_EXPR:
        std::cout << "*";
        break;
    case RDIV_EXPR:
        std::cout << "/";
        break;
    case BIT_IOR_EXPR:
        std::cout << "|";
        break;
    case BIT_NOT_EXPR:
        std::cout << "!";
        break;
    case TRUTH_AND_EXPR:
        std::cout << "&&";
        break;
    case TRUTH_OR_EXPR:
        std::cout << "||";
        break;
    case BIT_XOR_EXPR:
        std::cout << "^";
        break;
    case TRUTH_NOT_EXPR:
        std::cout << "!";
        break;
    case LT_EXPR:
        std::cout << "<";
        break;
    case LE_EXPR:
        std::cout << "<=";
        break;
    case GT_EXPR:
        std::cout << ">";
        break;
    case GE_EXPR:
        std::cout << ">=";
        break;
    case EQ_EXPR:
        std::cout << "==";
        break;
    case NE_EXPR:
        std::cout << "!=";
        break;
    default:
        std::cout << "UNKNOWN OP (" << code << ")";
        break;
    }
}


void on_gimple_assign(gimple* stmt) {
    std::cout << "\t\t" << "GIMPLE_ASSIGN: " << " { ";
    switch (gimple_num_ops(stmt)) {
    case 2:
        get_tree(gimple_assign_lhs(stmt));
        std::cout << " = ";
        get_tree(gimple_assign_rhs1(stmt));
        break;
    case 3:
        get_tree(gimple_assign_lhs(stmt));
        std::cout << " = ";
        get_tree(gimple_assign_rhs1(stmt));
        std::cout << " ";
        op(gimple_assign_rhs_code(stmt));
        std::cout << " ";
        get_tree(gimple_assign_rhs2(stmt));
        break;
    }
    std::cout << " }" << std::endl;
}


void on_gimple_call(gimple* stmt) {
    std::cout << "\t\t" << "GIMPLE_CALL: " << " { ";
    tree lhs = gimple_call_lhs (stmt);
    if (lhs) {
        get_tree(lhs);
        printf(" = ");
    }
    std::cout << fndecl_name(gimple_call_fndecl(stmt)) << "(";
    for (unsigned int i = 0; i < gimple_call_num_args(stmt); i++) {
        get_tree(gimple_call_arg(stmt, i));
        if (i != gimple_call_num_args(stmt) - 1) {
            std::cout << ", ";
        }
    }
    std::cout << ")";
    std::cout << " }" << std::endl;
}


void on_gimple_cond(gimple* stmt) {
    std::cout << "\t\t" << "GIMPLE_COND: "  << " { ";
    get_tree(gimple_cond_lhs(stmt));
    std::cout << " ";
    op(gimple_assign_rhs_code(stmt));
    std::cout << " ";
    get_tree(gimple_cond_rhs(stmt));
    std::cout << " }" << std::endl;
}

void on_gimple_label() {
    std::cout << "\t\t" << "GIMPLE_LABEL: " << " {";
    std::cout << "}" << std::endl;
}

void on_gimple_return() {
    std::cout << "\t\t" << "GIMPLE_RETURN: " << " {";
    std::cout << "}" << std::endl;
}

void statements(basic_block bb) {
    std::cout << "\t" << "statements:\n";
    for (gimple_stmt_iterator gsi = gsi_start_bb(bb); !gsi_end_p(gsi); gsi_next(&gsi)) {
        gimple* stmt = gsi_stmt(gsi);

        switch (gimple_code(stmt)) {
        case GIMPLE_ASSIGN:
            on_gimple_assign(stmt);
            break;
        case GIMPLE_CALL:
            on_gimple_call(stmt);
            break;
        case GIMPLE_COND:
            on_gimple_cond(stmt);
            break;
        case GIMPLE_LABEL:
            on_gimple_label();
            break;
        case GIMPLE_RETURN:
            on_gimple_return();
            break;
        }
    }
}

int based(function* fn) {
    std::cout << "\nfunc " << function_name(fn) << ":" << std::endl;
    basic_block bb;
    
    FOR_EACH_BB_FN(bb, fn) {
        bb_info(bb);
        statements(bb);
    }
    return 0;
}


struct pass : gimple_opt_pass {
    // constructor for opt_pass (const pass_data&, gcc::context *);
    pass(gcc::context *ctx) : gimple_opt_pass({GIMPLE_PASS,"lab1"}, ctx) {}

    /* This is the code to run.  If this is not overridden, then there should
   be sub-passes otherwise this pass does nothing.*/
    virtual unsigned int execute(function* fn) override {
        return based(fn);
    };
};


int plugin_init(struct plugin_name_args *args, struct plugin_gcc_version *version) {
    if(!plugin_default_version_check(version, &gcc_version)) {
        return 1;
    }

    register_pass_info pass_info = {new pass(g),"ssa",1,PASS_POS_INSERT_AFTER};

    register_callback(args->base_name, PLUGIN_PASS_MANAGER_SETUP, NULL, &pass_info);

    return 0;
}
