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
#include "parser_types.hpp"
#include "pstring.hpp"

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
    token_t token = { TOK_ERROR };
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
    template<typename Func> 
    unsigned parse_args(token_type_t l, token_type_t r, Func parse_func);

    // Parses an indented block OR a non-indented newline.
    template<typename Func>
    void maybe_parse_block(int parent_indent, Func func);

    // Parses an indented block and errors if it can't.
    template<typename Func>
    void parse_block(int const parent_indent, Func func);

    void expect_token(token_type_t expecting) const;
    bool parse_token(token_type_t expecting);
    bool parse_token();
    bool parse_indented_token();
    bool parse_line_ending();

    pstring_t parse_ident();
    pstring_t parse_group_ident();
    expr_temp_t parse_expr();
    expr_temp_t parse_expr_then();
    void parse_expr(expr_temp_t&, int starting_indent, int open_parens);

    type_t parse_type(bool allow_void);

    var_decl_t parse_var_decl();
    bool parse_var_init(var_decl_t& var_decl, expr_temp_t& expr);

    void parse_top_level();
    void parse_top_level_def();

    void parse_vars_group();
    void parse_fn();
    void parse_mode();

    void parse_statement();
    void parse_flow_statement();
    void parse_block_statement(int parent_indent);
    void parse_expr_statement();
    void parse_var_init_statement();
    void parse_if();
    void parse_do_while();
    void parse_while();
    void parse_for();
    void parse_return();
    void parse_break();
    void parse_continue();
    void parse_goto();
    void parse_label();

    [[gnu::noreturn]] void compiler_error(std::string const& what) const
        { ::compiler_error(file, token.pstring, what); }
};

template<typename Policy>
void parse(file_contents_t& file)
{
    Policy policy(file);
    parser_t parser(policy, file);
    parser.parse();
}

#endif
