#ifndef PARSER_HPP
#define PARSER_HPP

// Parser overview:
// - Recursive descent with no/minimal backtracking.
// - Does _NOT_ generate an AST itself.
//   - Instead 'callbacks' are called when parts of the grammar are parsed.
//   - These 'callbacks' take the form of template-driven policies.
// - Throws on error, which GREATLY simplifies the logic. 
//   - Recovering from parse errors takes a lot of work and complexity. KISS!

#include "compiler_error.hpp"
#include "file.hpp"
#include "parser_decl.hpp"
#include "token.hpp"
#include "pstring.hpp"
#include "asm_lex_tables.hpp"
#include "ast.hpp"

struct mods_t;

template<typename Policy>
class parser_t
{
public:
    using policy_type = Policy;

private:
    Policy* policy_ptr;
    file_contents_t const& file;

    char const* line_source = nullptr;
    char const* token_source = nullptr;
    char const* next_char = nullptr;
    token_t token = { lex::TOK_ERROR };
    int indent = 0;
    unsigned line_number = 0;

public:
    parser_t() = delete;
    parser_t(Policy& policy, file_contents_t const& file);

    void parse() { parse_top_level(); }

private:
    char const* source() const { return file.source(); }
    unsigned file_i() const { return file.index(); }
    Policy& policy() { return *policy_ptr; }

    // Parses comma-separated values between token types 'l' and 'r'.
    template<bool TrailingComma = false, typename Func> 
    unsigned parse_args(lex::token_type_t l, lex::token_type_t r, Func parse_func);

    // Parses an indented block OR a non-indented newline.
    template<typename Func>
    void maybe_parse_block(int parent_indent, Func func);

    // Parses an indented block and errors if it can't.
    template<typename Func>
    void parse_block(int const parent_indent, Func func);

    // Parses 'fn' as the start of a block, then parses and returns a mods.
    // This properly handles split lines!
    template<typename Fn>
    std::unique_ptr<mods_t> parse_mods_after(Fn const& fn);

    static bool fail_using() { return false; }

    void expect_token(lex::token_type_t expecting) const;
    bool parse_token(lex::token_type_t expecting);
    bool parse_token();
    asm_lex::token_type_t parse_asm_token(pstring_t from);
    bool parse_indented_token();
    bool parse_line_ending();
    void mill_eol();

    pstring_t parse_ident();
    string_literal_t parse_string_literal(bool open_parens, lex::token_type_t first = lex::TOK_dquote, char last = '"');
    string_literal_t parse_char_literal(bool open_parens);
    ast_node_t parse_string_or_char_expr(bool open_parens);
    pstring_t parse_group_ident();

    static std::uint16_t get_hw_reg(lex::token_type_t token_type);
    std::uint16_t parse_hw_reg();

    ast_node_t parse_expr_atom(int starting_indent, int open_parens);
    ast_node_t parse_expr(int starting_indent, int open_parens, int min_precedence = 256);
    ast_node_t parse_expr();

    /*
    expr_temp_t parse_expr();
    void parse_expr(expr_temp_t&, int starting_indent, int open_parens);
    */
    ast_node_t parse_cast(src_type_t& src_type, int open_parens=0);

    src_type_t parse_type(bool allow_void, bool allow_bank_size, group_ht group, 
                          bool allow_groupless_paa = false);

    var_decl_t parse_var_decl(bool block_init, group_ht group, bool allow_groupless_paa = false);
    bool parse_var_init(var_decl_t& var_decl, ast_node_t& expr, std::unique_ptr<mods_t>* mods, 
                        global_t** block_init_global, group_ht group, bool is_banked);

    template<typename Children>
    bool parse_byte_block(pstring_t decl, int block_indent, global_t& global, bool is_banked, Children& children);
    ast_node_t parse_byte_block(pstring_t decl, int block_indent, global_t& global, bool is_banked);

    std::unique_ptr<mods_t> parse_mods(int base_indent);

    template<typename Fn>
    auto parse_file(lex::token_type_t tt, Fn const& fn);

    void parse_top_level();
    void parse_top_level_def();

    void parse_const();
    void parse_group_vars();
    void parse_group_data();
    void parse_fn(lex::token_type_t prefix = {});
    void parse_with();
    void parse_struct();
    void parse_charmap();
    void parse_chrrom();
    void parse_audio();

    void parse_statement();
    void parse_flow_statement();
    void parse_block_statement(int parent_indent);
    void parse_expr_statement();
    void parse_var_init_statement();
    void parse_if();
    void parse_do();
    void parse_while(bool is_do = false);
    void parse_for(bool is_do = false);
    void parse_return();
    void parse_break();
    void parse_continue();
    void parse_goto();
    void parse_label();
    void parse_case();
    void parse_default();
    void parse_switch();
    void parse_nmi_statement();
    void parse_fence();
    void parse_local_ct();

    // TODO: remove
    //void parse_asm_local_const();
    //void parse_asm_label_block();
    //void parse_asm_op();

    [[gnu::noreturn]] void compiler_error(pstring_t pstring, std::string const& what) const
        { ::compiler_error(pstring, what, &file); }
    [[gnu::noreturn]] void compiler_error(std::string const& what) const
        { ::compiler_error(token.pstring, what, &file); }

    void compiler_warning(pstring_t pstring, std::string const& what) const
        { ::compiler_warning(pstring, what, &file); }
    void compiler_warning(std::string const& what) const
        { ::compiler_warning(token.pstring, what, &file); }
};

template<typename Policy>
void parse(file_contents_t& file)
{
    Policy policy(file);
    parser_t parser(policy, file);
    parser.parse();
}

#endif
