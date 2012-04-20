-------------------------------------------------------------------------------
-- Copyright (c) 2012 Andrew Udvare <audvare@gmail.com>
--
-- This grammar is free software; you can redistribute it and/or
-- modify it under the terms of the GNU Library General Public
-- License as published by the Free Software Foundation; either
-- version 2 of the License, or (at your option) any later version.
--
-- This grammar is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
-- Lesser General Public License for more details.
--
-- You should have received a copy of the GNU Library General Public License
-- along with this library; see the file COPYING.LIB.  If not, write to
-- the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
-- Boston, MA 02110-1301, USA.
-----------------------------------------------------------

-----------------------------------------------------------
-- Grammar for ECMAScript 3 and 5
-- Parts taken from KDevelop PHP Grammar
-----------------------------------------------------------

------------------------------------------------------------
-- Forward declaration in jsast.h
------------------------------------------------------------

[:

namespace KDevelop
{
    class DUContext;
}

:]

------------------------------------------------------------
-- Additional includes for the parser
------------------------------------------------------------

%parser_declaration_header "tokenstream.h"
%parser_declaration_header "QtCore/QString"
%parser_declaration_header "language/interfaces/iproblem.h"
%parser_declaration_header "jslexer.h"

%parser_bits_header "kdebug.h"

------------------------------------------------------------
-- Export macro to use the parser in a shared lib
------------------------------------------------------------
%export_macro "KDEVJSPARSER_EXPORT"
%export_macro_header "parserexport.h"


------------------------------------------------------------
-- Enumeration types for additional AST members,
-- in the global "Js" namespace
------------------------------------------------------------
%namespace
[:
    class Lexer;

    // Technically object
    enum ClassModifier {
        NormalClass
    };

    enum ScalarTypes {
        ScalarTypeInt,
        ScalarTypeFloat,
        ScalarTypeString
    };

    enum OperationType {
        OperationPlus = 1,
        OperationMinus,
        OperationMul,
        OperationDiv,
        OperationMod,
        OperationAnd,
        OperationOr,
        OperationXor,
        OperationSl,
        OperationSr,
        OperationSrr
    };
:]

------------------------------------------------------------
-- Ast Node class members
------------------------------------------------------------
%ast_extra_members
[:
  KDevelop::DUContext* ducontext;
:]

------------------------------------------------------------
-- Parser class members
------------------------------------------------------------

%parserclass (public declaration)
[:
  /**
   * Transform the raw input into tokens.
   * When this method returns, the parser's token stream has been filled
   * and any parse*() method can be called.
   */
  void tokenize(const QString& contents, int initialState = Lexer::HtmlState);

  enum ProblemType {
      Error,
      Warning,
      Info
  };
  void reportProblem( Parser::ProblemType type, const QString& message, int tokenOffset = -1 );
  QList<KDevelop::ProblemPointer> problems() {
      return m_problems;
  }
  QString tokenText(qint64 begin, qint64 end);
  void setDebug(bool debug);
  void setCurrentDocument(KDevelop::IndexedString url);

    enum InitialLexerState {
        HtmlState = 0,
        DefaultState = 1
    };

:]

%parserclass (private declaration)
[:
    enum VarExpressionState {
        Normal,
        OnlyVariable,
        OnlyNewObject
    };
    QString m_contents;
    bool m_debug;
    KDevelop::IndexedString m_currentDocument;
    QList<KDevelop::ProblemPointer> m_problems;

    struct ParserState {
        VarExpressionState varExpressionState;
        bool varExpressionIsVariable;
    };
    ParserState m_state;
:]

%parserclass (constructor)
[:
    m_state.varExpressionState = Normal;
    m_state.varExpressionIsVariable = false;
:]


%token_stream TokenStream ;;

-----------------------------------------------------------
-- List of defined tokens
-----------------------------------------------------------

-- Based on https://developer.mozilla.org/en/JavaScript/Reference/Reserved_Words
-- keywords:
%token BREAK ("break"), CASE ("case"), CATCH ("catch"),
       CLASS ("class"), ENUM ("enum"), CONST ("const"), CONTINUE ("continue"),
       DEBUGGER ("debugger"), DELETE ("delete"), DEFAULT ("default"), DO ("do"),
       ELSE ("else"), EXPORT ("export"), EXTENDS ("extends"), FINALLY ("finally"),
       FOR ("for"), IF ("if"), IMPLEMENTS ("implements"), IMPORT ("import"),
       INSTANCEOF ("instanceof"), INTERFACE ("interface"), LET ("let"), NEW ("new"),
       PACKAGE ("package"), PRIVATE ("private"), PROTECTED ("protected"), PUBLIC ("public"),
       RETURN ("return"), STATIC ("static"), SUPER ("super"), SWITCH ("switch"), THIS ("this"),
       THROW ("throw"), TRY ("try"), TYPEOF ("typeof"), WHILE ("while"), WITH ("with"), IN ("in"),
       FUNCTION ("function"), VAR ("var"), VOID ("void"), YIELD ("yield"),
       COMMENT ("comment") ;;

-- seperators:
%token SEMICOLON (";"), DOUBLE_QUOTE ("\""), LBRACKET ("["),
       RBRACKET ("]"),
       LPAREN ("("), RPAREN (")"), LBRACE ("{"), RBRACE ("}"),
       COMMA (","),
       ASSUMED_SEMICOLON ("\n")

-- operators:
%token IS_EQUAL ("=="), IS_NOT_EQUAL ("!="), IS_IDENTICAL ("==="),
       IS_NOT_IDENTICAL ("!=="), IS_SMALLER ("<"), IS_GREATER (">"),
       IS_SMALLER_OR_EQUAL ("<="), IS_GREATER_OR_EQUAL (">="),
       BOOLEAN_OR ("||"), BOOLEAN_AND ("&&"), ASSIGN ("="),
       PLUS_ASSIGN ("+="), MINUS_ASSIGN ("-="), MUL_ASSIGN ("*="), DIV_ASSIGN ("/="),
       MOD_ASSIGN ("%="), AND_ASSIGN ("&="), OR_ASSIGN ("|="),
       XOR_ASSIGN ("^="), SL_ASSIGN ("<<="), SR_ASSIGN (">>="), OBJECT_OPERATOR ("."),
       PLUS ("+"), MINUS("-"),
       INC ("++"), DEC ("--"), BANG ("!"), QUESTION ("?"), COLON (":"),
       BIT_AND ("&"), BIT_OR("|"), BIT_XOR ("^"),
       SL ("<<"), SR (">>"), SRR (">>>"), MUL("*"), DIV("/"), MOD ("%"),
       TILDE ("~"),
       LOGICAL_OR ("logical or"), LOGICAL_AND ("logical and"), LOGICAL_XOR ("logical xor") ;;

-- literals and identifiers:
%token WHITESPACE ("whitespace"),
       VARIABLE ("variable"),
       DNUMBER ("double number"), LNUMBER ("long number"),
       NUM_STRING ("num string"), STRING ("string")

-- token that makes the parser fail in any case:
%token INVALID ("invalid token") ;;

-- The actual grammar starts here.

#statements=outerTopStatement*
-> start ;;

  statement=topStatement
-> outerTopStatement ;;

-- first/first conflict for FUNCTION
   (?[: (LA(1).kind == Token_FUNCTION && ((LA(2).kind == Token_BIT_AND && LA(3).kind == Token_LPAREN)
            || LA(2).kind == Token_LPAREN))
        || LA(1).kind != Token_FUNCTION :]
    statement=statement )
   | functionDeclaration=functionDeclarationStatement
-> topStatement ;;

[: bool reported = false; while ( true ) { :]
  try/recover(#statements=topStatement)*
[: if (yytoken != Token_RBRACE && yytoken != Token_EOF && yytoken != Token_CLOSE_TAG
       && yytoken != Token_ELSE
       && yytoken != Token_CASE && yytoken != Token_DEFAULT) {
        if (!reported) {
            qint64 index = tokenStream->index() - 1;
            Token &token = tokenStream->at(index);
            QString tokenValue = token.kind != 0 ? tokenText(token.begin, token.end) : "EOF";
            reportProblem(Error, QString ("Unexpected token \"%1\".").arg(tokenValue));
            reported = true;
        }
        yylex();
   } else {
        break;
   }
} :]
-> innerStatementList ;;

--Operator precendence, according to MDN
--https://developer.mozilla.org/en/JavaScript/Reference/Operators/Operator_Precedence
--left     , comma
--right    = += -= *= /= .= %= &= |= ^= <<= >>= >>>=   assignment
--right    ? : conditional
--left    ||  logical
--left    &&  logical
--left    |   bitwise
--left    ^   bitwise
--left    &   bitwise
--left    == != === !==   equality
--left    < <= > >=   relational, in, instanceof
--left    << >> >>>  bitwise
--left    + -   arithmetic
--left    * / %   arithmetic
--right   ! ~ + - typeof void delete
--none    ++ -- increment/decrement
--left    ()   function call
--right   new new
--left    . member

  (print=PRINT*) expression=assignmentExpression
-> printExpression ;;

ASSIGN
    assignmentExpressionCheckIfVariable --as in assignmentExpression
    (BIT_AND [: m_state.varExpressionState = OnlyVariable; :] -- wrong?
     | 0) assignmentExpression=assignmentExpression [: m_state.varExpressionState = Normal; :]
-> assignmentExpressionEqual ;;

-- leftside must be a variable, we check afterwards if it was a variable and
-- if not we report an error
0 --needed for line below
[: m_state.varExpressionIsVariable = false; :] --reset flag
expression=conditionalExpression
(
  assignmentExpressionEqual=assignmentExpressionEqual | (
     (
        PLUS_ASSIGN   [: (*yynode)->operation = OperationPlus; :]
      | MINUS_ASSIGN  [: (*yynode)->operation = OperationMinus; :]
      | MUL_ASSIGN    [: (*yynode)->operation = OperationMul; :]
      | DIV_ASSIGN    [: (*yynode)->operation = OperationDiv; :]
      | MOD_ASSIGN    [: (*yynode)->operation = OperationMod; :]
      | AND_ASSIGN    [: (*yynode)->operation = OperationAnd; :]
      | OR_ASSIGN     [: (*yynode)->operation = OperationOr; :]
      | XOR_ASSIGN    [: (*yynode)->operation = OperationXor; :]
      | SL_ASSIGN     [: (*yynode)->operation = OperationSl; :]
      | SR_ASSIGN     [: (*yynode)->operation = OperationSr; :]
      | SRR_ASSIGN    [: (*yynode)->operation = OperationSrr; :]
     )
     assignmentExpressionCheckIfVariable
     assignmentExpression=assignmentExpression)
   | 0)
-> assignmentExpression [
     member variable operation: OperationType;
];;

-- check if var_expression was a variable, if not report an error
-- varExpressionIsVariable is set in var_expression
0 --to allow cpp-code
[:
    if (!m_state.varExpressionIsVariable) {
        reportProblem(Error, "Left side is not a variable");
        return false;
    }
:]
-> assignmentExpressionCheckIfVariable ;;

   #expression=logicalXorExpression @ LOGICAL_OR
-> logicalOrExpression ;;

   #expression=printExpression @ LOGICAL_AND
-> logicalAndExpression ;;

   #expression=bitXorExpression @ BIT_OR
-> bitOrExpression ;;

   #expression=bitAndExpression @ BIT_XOR
-> bitXorExpression ;;

   #expression=equalityExpression @ BIT_AND
-> bitAndExpression ;;

   expression=relationalExpression
   (#additionalExpression=equalityExpressionRest)*
-> equalityExpression ;;

   (  IS_EQUAL | IS_NOT_EQUAL | IS_IDENTICAL | IS_NOT_IDENTICAL )
   expression=relationalExpression
-> equalityExpressionRest ;;

   ( IS_SMALLER | IS_GREATER | IS_SMALLER_OR_EQUAL | IS_GREATER_OR_EQUAL )
   expression=shiftExpression
-> relationalExpressionRest ;;

   expression=shiftExpression
   (  (#additionalExpression=relationalExpressionRest)+
      --instanceof as in java.g (correct??)
    | INSTANCEOF instanceofType=classNameReference
    | IN inType=classNameReference
    | 0
   )
-> relationalExpression ;;

   ( SL | SR | SRR )
   expression=additiveExpression
-> shiftExpressionRest ;;

   expression=multiplicativeExpression
   (#additionalExpression=additiveExpressionRest)*
-> additiveExpression ;;

   (
       PLUS   [: (*yynode)->operation = OperationPlus; :]
     | MINUS  [: (*yynode)->operation = OperationMinus; :]
   )
   expression=multiplicativeExpression
-> additiveExpressionRest [
     member variable operation: OperationType;
];;

   expression=unaryExpression
   (#additionalExpression=multiplicativeExpressionRest)*
-> multiplicativeExpression ;;

   (
       MUL [: (*yynode)->operation = OperationMul; :]
     | DIV [: (*yynode)->operation = OperationDiv; :]
     | MOD [: (*yynode)->operation = OperationMod; :]
   )
   expression=unaryExpression
-> multiplicativeExpressionRest [
     member variable operation: OperationType;
];;

 (
    MINUS unaryExpression=unaryExpression
  | PLUS  unaryExpression=unaryExpression
  | BANG unaryExpression=unaryExpression
  | TILDE unaryExpression=unaryExpression
  | unaryExpressionNotPlusminus=unaryExpressionNotPlusminus
 )
-> unaryExpression [
     member variable castType: CastType;
];;

    (#prefixOperator=postprefixOperator)*
    varExpression=varExpression
    (#postfixOperator=postprefixOperator)*
-> unaryExpressionNotPlusminus ;;

   op=INC | op=DEC
-> postprefixOperator ;;

--first/first conflict - no problem because of ifs
    ?[: m_state.varExpressionState == OnlyVariable :] 0 [: m_state.varExpressionState = Normal; :] variable=variable
  | ?[: m_state.varExpressionState == OnlyNewObject :] 0 [: m_state.varExpressionState = Normal; :] newObject=varExpressionNewObject
  | varExpressionNormal=varExpressionNormal
-> varExpression ;;

    LPAREN expression=expr RPAREN
  --try/rollback resolves conflict scalar vs. staticMember (foo::bar vs. foo::$bar)
  --varExpressionIsVariable flag is needed for assignmentExpression
  | try/rollback (variable=variable [: m_state.varExpressionIsVariable = true; :])
    catch (scalar=scalar)
  | newObject=varExpressionNewObject
  | closure=closure
-> varExpressionNormal ;;

    FUNCTION (isRef=BIT_AND|0) LPAREN parameters=parameterList RPAREN
        LBRACE try/recover(functionBody=innerStatementList) RBRACE
-> closure ;;

  (isRef=BIT_AND | 0) variable=variableIdentifier
-> lexicalVar ;;

    NEW className=classNameReference ctor=ctorArguments
-> varExpressionNewObject ;;

    LPAREN parameterList=functionCallParameterList RPAREN
  | 0
-> ctorArguments ;;

    #parameters=functionCallParameterListElement @ COMMA | 0
-> functionCallParameterList ;;

    #element=assignmentListElement @COMMA
-> assignmentList ;;

   var=baseVariableWithFunctionCalls (#variableProperties=variableProperty*)
-> variable ;;

    (OBJECT_OPERATOR)
    ( objectProperty=objectProperty )
    (isFunctionCall=LPAREN parameterList=functionCallParameterList RPAREN | 0)
-> variableProperty ;;

   --Conflict
   --   foo.bar[0] (=baseVariable-staticMember)
   --vs.foo.bar[0](); (=static function call)
   try/rollback (functionCall=functionCall)
   catch (baseVariable=baseVariable)
-> baseVariableWithFunctionCalls ;;

    stringFunctionNameOrClass=namespacedIdentifier (
        LPAREN stringParameterList=functionCallParameterList RPAREN
      | OBJECT_OPERATOR
        (
            stringFunctionName=identifier LPAREN stringParameterList=functionCallParameterList RPAREN
            | varFunctionName=variableWithoutObjects LPAREN stringParameterList=functionCallParameterList RPAREN
        )
    )
  | varFunctionName=variableWithoutObjects LPAREN varParameterList=functionCallParameterList RPAREN
-> functionCall ;;

    var=compoundVariableWithSimpleIndirectReference #offsetItems=dimListItem*
  | staticMember=staticMember
-> baseVariable ;;

  ( indirectVariable=variableIdentifier | LBRACE expr=expr RBRACE ) | variable=variableIdentifier )
-> compoundVariableWithSimpleIndirectReference ;;

    expr=expr | 0
-> dimOffset ;;

    className=namespacedIdentifier OBJECT_OPERATOR variable=variableWithoutObjects
-> staticMember ;;

    LBRACE try/recover(statements=innerStatementList) RBRACE
  | IF LPAREN ifExpr=expr RPAREN
      (   COLON statements=innerStatementList newElseSingle semicolonOrCloseTag
        | ifStatement=statement elseifList=elseifList elseSingle=elseSingle
      )
  | WHILE LPAREN whileExpr=expr RPAREN whileStatement=whileStatement
  | FOR LPAREN forExpr1=forExpr SEMICOLON forExpr2=forExpr
    SEMICOLON forExpr3=forExpr RPAREN forStatement=forStatement
  | SWITCH LPAREN swtichExpr=expr RPAREN switchCaseList=switchCaseList

  | SEMICOLON     -- empty statement
  | TRY  LBRACE try/recover(statements=innerStatementList) RBRACE
    #catches=catchItem*
  | ( ?[: LA(1).kind != Token_STRING || LA(2).kind != Token_COLON :] expr=expr semicolonOrCloseTag )
  | DO doStatement=statement WHILE LPAREN whileExpr=expr RPAREN semicolonOrCloseTag
  | BREAK (breakExpr=expr | 0) semicolonOrCloseTag
  | CONTINUE (continueExpr=expr | 0) semicolonOrCloseTag
  | RETURN (returnExpr=expr | 0) semicolonOrCloseTag
  | THROW throwExpr=expr semicolonOrCloseTag
  -- throws error in zend parser, so ignored | USE use_filename  semicolonOrCloseTag

  | CONST #consts=constantDeclaration @ COMMA SEMICOLON
-> statement ;;

    identifier=namespacedIdentifier (AS aliasIdentifier=identifier | 0)
-> useNamespace ;;

    identifier=identifier ASSIGN scalar=staticScalar
-> constantDeclaration ;;

   SEMICOLON | CLOSE_TAG
-> semicolonOrCloseTag ;;

    LBRACE (SEMICOLON | 0) try/recover(caseList=caseList) RBRACE
  | COLON (SEMICOLON | 0) caseList=caseList semicolonOrCloseTag
-> switchCaseList ;;

    #caseItems=case_item*
-> caseList ;;

    CASE expr=expr (COLON | SEMICOLON) statements=innerStatementList
  | def=DEFAULT (COLON | SEMICOLON) statements=innerStatementList
-> case_item ;;

    CATCH LPAREN catchClass=namespacedIdentifier var=variableIdentifier RPAREN
    LBRACE try/recover(statements=innerStatementList) RBRACE
-> catchItem ;;

    #exprs=expr @ COMMA | 0
-> forExpr ;;

    statement=statement
  | COLON statements=innerStatementList ENDFOR semicolonOrCloseTag
-> forStatement ;;

    statement=statement
  | COLON statements=innerStatementList ENDWHILE semicolonOrCloseTag
-> whileStatement ;;

    --first/follow conflict; todo check if this is a problem
    #elseifListItem=elseifListItem*
-> elseifList ;;

    ELSE statement=statement | 0
-> elseSingle ;;

    #newElseifListItem=newelseifListItem*
-> newElseifList ;;

    ELSE COLON statements=innerStatementList | 0
-> newElseSingle ;;

--TODO     --resolve STRING vs. staticMember conflict
--     ?[: LA(2).kind != Token_OBJECT_OPERATOR :]
    identifier=namespacedIdentifier
-> classNameReference ;;

    baseVariable=baseVariable (OBJECT_OPERATOR objectProperty=objectProperty
                          properties=dynamicClassNameVariableProperties | 0)
-> dynamicClassNameReference ;;

    #properties=dynamicClassNameVariableProperty*
-> dynamicClassNameVariableProperties ;;

    OBJECT_OPERATOR property=objectProperty
-> dynamicClassNameVariableProperty ;;

    objectDimList=objectDimList
  | variableWithoutObjects=variableWithoutObjects
-> objectProperty ;;

    variableName=variableName #offsetItems=dimListItem*
-> objectDimList ;;

  variable=compoundVariableWithSimpleIndirectReference #offsetItems=dimListItem*
-> variableWithoutObjects ;;

LBRACKET dimOffset=dimOffset RBRACKET | LBRACE expr=expr RBRACE
-> dimListItem ;;

    name=identifier
  | LBRACE expr=expr RBRACE
-> variableName ;;

    commonScalar=commonScalar
  | constantOrClassConst=constantOrClassConst
  | varname=STRING_VARNAME
  | DOUBLE_QUOTE encapsList=encapsList DOUBLE_QUOTE
-> scalar ;;

  constant=namespacedIdentifier
  ( PAAMAYIM_NEKUDOTAYIM classConstant=identifier | 0 )
-> constantOrClassConst ;;

    LNUMBER                  [: (*yynode)->scalarType = ScalarTypeInt; :]
  | DNUMBER                  [: (*yynode)->scalarType = ScalarTypeFloat; :]
  | string=CONSTANT_ENCAPSED_STRING [: (*yynode)->scalarType = ScalarTypeString; :]
-> commonScalar [
     member variable scalarType: ScalarTypes;
] ;;

    FUNCTION (BIT_AND | 0) functionName=identifier
    LPAREN parameters=parameterList RPAREN LBRACE try/recover(functionBody=innerStatementList) RBRACE
-> functionDeclarationStatement ;;

    (#parameters=parameter @ COMMA) | 0
-> parameterList ;;

(isRef=BIT_AND | 0)
    variable=variableIdentifier (ASSIGN defaultValue=staticScalar | 0)
-> parameter ;;

    value=commonScalar
  | constantOrClassConst=constantOrClassConst
  | PLUS plusValue=staticScalar
  | MINUS minusValue=staticScalar
-> staticScalar ;;

-- technically Object
    #val1=staticScalar (DOUBLE_ARROW #val2=staticScalar | 0)
-> staticArrayPairValue ;;

     string=STRING
-> identifier ;;

     variable=VARIABLE
-> variableIdentifier ;;

-----------------------------------------------------------------
-- Code segments copied to the implementation (.cpp) file.
-- If existent, kdevelop-pg's current syntax requires this block
-- to occur at the end of the file.
-----------------------------------------------------------------

[:

#include <QtCore/QDebug>
#include <KTextEditor/Range>

namespace Js {

void Parser::tokenize(const QString& contents, int initialState)
{
    m_contents = contents;
    Lexer lexer(tokenStream, contents, initialState);
    int kind = Parser::Token_EOF;
    int lastDocCommentBegin;
    int lastDocCommentEnd;

    do
    {
        lastDocCommentBegin = 0;
        lastDocCommentEnd = 0;
        kind = lexer.nextTokenKind();
        while (kind == Parser::Token_WHITESPACE || kind == Parser::Token_COMMENT || kind == Parser::Token_DOC_COMMENT) {
            if (kind == Parser::Token_DOC_COMMENT) {
                lastDocCommentBegin = lexer.tokenBegin();
                lastDocCommentEnd = lexer.tokenEnd();
            }
            kind = lexer.nextTokenKind();
        }
        if ( !kind ) // when the lexer returns 0, the end of file is reached
        {
            kind = Parser::Token_EOF;
        }
        Parser::Token &t = tokenStream->push();
        t.begin = lexer.tokenBegin();
        t.end = lexer.tokenEnd();
        t.kind = kind;
        t.docCommentBegin = lastDocCommentBegin;
        t.docCommentEnd = lastDocCommentEnd;
        //if ( m_debug ) qDebug() << kind << tokenText(t.begin,t.end) << t.begin << t.end;
    }
    while ( kind != Parser::Token_EOF );

    yylex(); // produce the look ahead token
}

QString Parser::tokenText(qint64 begin, qint64 end)
{
    return m_contents.mid(begin,end-begin+1);
}


void Parser::reportProblem( Parser::ProblemType type, const QString& message, int offset )
{
    if (type == Error)
        qDebug() << "** ERROR:" << message;
    else if (type == Warning)
        qDebug() << "** WARNING:" << message;
    else if (type == Info)
        qDebug() << "** Info:" << message;

    qint64 sLine;
    qint64 sCol;
    qint64 index = tokenStream->index() + offset;
    tokenStream->startPosition(index, &sLine, &sCol);
    qint64 eLine;
    qint64 eCol;
    tokenStream->endPosition(index, &eLine, &eCol);
    KDevelop::Problem *p = new KDevelop::Problem();
    p->setSource(KDevelop::ProblemData::Parser);
    switch ( type ) {
        case Error:
            p->setSeverity(KDevelop::ProblemData::Error);
            break;
        case Warning:
            p->setSeverity(KDevelop::ProblemData::Warning);
            break;
        case Info:
            p->setSeverity(KDevelop::ProblemData::Hint);
            break;
    }
    p->setDescription(message);
    p->setFinalLocation(KDevelop::DocumentRange(m_currentDocument, KDevelop::SimpleRange(sLine, sCol, eLine, eCol+1)));
    m_problems << KDevelop::ProblemPointer(p);
}


// custom error recovery
void Parser::expectedToken(int /*expected*/, qint64 /*where*/, const QString& name)
{
    reportProblem( Parser::Error, QString("Expected token \"%1\"").arg(name));
}

void Parser::expectedSymbol(int /*expectedSymbol*/, const QString& name)
{
    qint64 line;
    qint64 col;
    qint64 index = tokenStream->index()-1;
    Token &token = tokenStream->at(index);
    kDebug() << "token starts at:" << token.begin;
    kDebug() << "index is:" << index;
    tokenStream->startPosition(index, &line, &col);
    QString tokenValue = tokenText(token.begin, token.end);
    qint64 eLine;
    qint64 eCol;
    tokenStream->endPosition(index, &eLine, &eCol);
    reportProblem( Parser::Error,
                   QString("Expected symbol \"%1\" (current token: \"%2\" [%3] at %4:%5 - %6:%7)")
                  .arg(name)
                  .arg(token.kind != 0 ? tokenValue : "EOF")
                  .arg(token.kind)
                  .arg(line)
                  .arg(col)
                  .arg(eLine)
                  .arg(eCol));
}

void Parser::setDebug( bool debug )
{
    m_debug = debug;
}

void Parser::setCurrentDocument(KDevelop::IndexedString url)
{
    m_currentDocument = url;
}


Parser::ParserState *Parser::copyCurrentState()
{
    ParserState *state = new ParserState();
    state->varExpressionState = m_state.varExpressionState;
    state->varExpressionIsVariable = m_state.varExpressionIsVariable;
    return state;
}

void Parser::restoreState( Parser::ParserState* state)
{
    m_state.varExpressionState = state->varExpressionState;
    m_state.varExpressionIsVariable = state->varExpressionIsVariable;
}

} // end of namespace Js

:]

-- kate: space-indent on; indent-width 4; tab-width 4; replace-tabs on; auto-insert-doxygen on; mode KDevelop-PG[-Qt]
