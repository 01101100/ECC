/* 
 * @copyright (c) 2008, Hedspi, Hanoi University of Technology
 * @author Huu-Duc Nguyen
 * @version 1.0
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "reader.h"
#include "scanner.h"
#include "parser.h"
#include "semantics.h"
#include "error.h"
#include "debug.h"
#include "codegen.h"

Token *currentToken;
Token *lookAhead;

extern Type* intType;
extern Type* doubleType;
extern Type* stringType;
extern Type* charType;
extern SymTab* symtab;

void scan(void) {
  Token* tmp = currentToken;
  currentToken = lookAhead;
  lookAhead = getValidToken();
  free(tmp);
}

void eat(TokenType tokenType) {
  if (lookAhead->tokenType == tokenType) {
    //    printToken(lookAhead);
    scan();
  } else missingToken(tokenType, lookAhead->lineNo, lookAhead->colNo);
}

void compileProgram(void) {
  Object* program;

  eat(KW_PROGRAM);
  eat(TK_IDENT);

  program = createProgramObject(currentToken->string);
  program->progAttrs->codeAddress = getCurrentCodeAddress();
  enterBlock(program->progAttrs->scope);

  eat(SB_SEMICOLON);

  compileBlock();
  eat(SB_PERIOD);

  genHL();

  exitBlock();
}

void compileConstDecls(void) {
  Object* constObj;
  ConstantValue* constValue;

  if (lookAhead->tokenType == KW_CONST) {
    eat(KW_CONST);
    do {
      eat(TK_IDENT);
      checkFreshIdent(currentToken->string);
      constObj = createConstantObject(currentToken->string);
      declareObject(constObj);
      
      eat(SB_EQ);
      constValue = compileConstant();
      constObj->constAttrs->value = constValue;
      
      eat(SB_SEMICOLON);
    } while (lookAhead->tokenType == TK_IDENT);
  }
}

void compileTypeDecls(void) {
  Object* typeObj;
  Type* actualType;

  if (lookAhead->tokenType == KW_TYPE) {
    eat(KW_TYPE);
    do {
      eat(TK_IDENT);
      
      checkFreshIdent(currentToken->string);
      typeObj = createTypeObject(currentToken->string);
      declareObject(typeObj);
      
      eat(SB_EQ);
      actualType = compileType();
      typeObj->typeAttrs->actualType = actualType;
      
      eat(SB_SEMICOLON);
    } while (lookAhead->tokenType == TK_IDENT);
  } 
}

void compileVarDecls(void) {
  Object* varObj;
  Type* varType, *type;
  int count = 0, i = 0;
  char ident[15][15];
  // LValueList *list, *currentNode, *node;
  if (lookAhead->tokenType == KW_VAR) {
    eat(KW_VAR);
    do {
      eat(TK_IDENT);
      checkFreshIdent(currentToken->string);
      varObj = createVariableObject(currentToken->string);
      // list = (LValueList*)malloc(sizeof(LValueList));
      // list->next = NULL;
      // strcpy(list->ident, currentToken->string);

      // currentNode = list;
      while(lookAhead->tokenType == SB_COMMA){
        eat(SB_COMMA);
        // node = (LValueList*)malloc(sizeof(LValueList));
        // node->next = NULL;
        // strcpy(node->ident, lookAhead->string);
        // currentNode->next = node;
        // currentNode = node;
        eat(TK_IDENT);
        strcpy(ident[count++], currentToken->string);
      }

      eat(SB_COLON);
      varType = compileType();
      varObj->varAttrs->type = varType;
      declareObject(varObj);  

      // currentNode = list->next;
      while(i < count){
        type = duplicateType(varType);
        checkFreshIdent(ident[i]);
        varObj = createVariableObject(ident[i]);
        varObj->varAttrs->type = type;
        declareObject(varObj);
        // currentNode = currentNode->next;
        i++;
      }    
      eat(SB_SEMICOLON);
    } while (lookAhead->tokenType == TK_IDENT);
  } 
}

void compileBlock(void) {
  Instruction* jmp;
  
  jmp = genJ(DC_VALUE);

  compileConstDecls();
  compileTypeDecls();
  compileVarDecls();
  compileSubDecls();

  updateJ(jmp,getCurrentCodeAddress());
  genINT(symtab->currentScope->frameSize);

  eat(KW_BEGIN);
  compileStatements();
  eat(KW_END);
}

void compileSubDecls(void) {
  while ((lookAhead->tokenType == KW_FUNCTION) || (lookAhead->tokenType == KW_PROCEDURE)) {
    if (lookAhead->tokenType == KW_FUNCTION)
      compileFuncDecl();
    else compileProcDecl();
  }
}

void compileFuncDecl(void) {
  Object* funcObj;
  Type* returnType;

  eat(KW_FUNCTION);
  eat(TK_IDENT);

  checkFreshIdent(currentToken->string);
  funcObj = createFunctionObject(currentToken->string);
  funcObj->funcAttrs->codeAddress = getCurrentCodeAddress();
  declareObject(funcObj);

  enterBlock(funcObj->funcAttrs->scope);
  
  compileParams();

  eat(SB_COLON);
  returnType = compileBasicType();
  funcObj->funcAttrs->returnType = returnType;

  eat(SB_SEMICOLON);

  compileBlock();

  genEF();
  eat(SB_SEMICOLON);

  exitBlock();
}

void compileProcDecl(void) {
  Object* procObj;

  eat(KW_PROCEDURE);
  eat(TK_IDENT);

  checkFreshIdent(currentToken->string);
  procObj = createProcedureObject(currentToken->string);
  procObj->procAttrs->codeAddress = getCurrentCodeAddress();
  declareObject(procObj);

  enterBlock(procObj->procAttrs->scope);

  compileParams();

  eat(SB_SEMICOLON);
  compileBlock();

  genEP();
  eat(SB_SEMICOLON);

  exitBlock();
}

ConstantValue* compileUnsignedConstant(void) {
  ConstantValue* constValue;
  Object* obj;

  switch (lookAhead->tokenType) {
  case TK_NUMBER:
    eat(TK_NUMBER);
    constValue = makeIntConstant(currentToken->value);
    break;
  case TK_IDENT:
    eat(TK_IDENT);

    obj = checkDeclaredConstant(currentToken->string);
    constValue = duplicateConstantValue(obj->constAttrs->value);

    break;
  case TK_CHAR:
    eat(TK_CHAR);
    constValue = makeCharConstant(currentToken->string[0]);
    break;
  default:
    error(ERR_INVALID_CONSTANT, lookAhead->lineNo, lookAhead->colNo);
    break;
  }
  return constValue;
}

ConstantValue* compileConstant(void) {
  ConstantValue* constValue;

  switch (lookAhead->tokenType) {
  case SB_PLUS:
    eat(SB_PLUS);
    constValue = compileConstant2();
    break;
  case SB_MINUS:
    eat(SB_MINUS);
    constValue = compileConstant2();
    constValue->intValue = - constValue->intValue;
    break;
  case TK_CHAR:
    eat(TK_CHAR);
    constValue = makeCharConstant(currentToken->string[0]);
    break;
  default:
    constValue = compileConstant2();
    break;
  }
  return constValue;
}

ConstantValue* compileConstant2(void) {
  ConstantValue* constValue;
  Object* obj;

  switch (lookAhead->tokenType) {
  case TK_NUMBER:
    eat(TK_NUMBER);
    constValue = makeIntConstant(currentToken->value);
    break;
  case TK_IDENT:
    eat(TK_IDENT);
    obj = checkDeclaredConstant(currentToken->string);
    if (obj->constAttrs->value->type == TP_INT)
      constValue = duplicateConstantValue(obj->constAttrs->value);
    else
      error(ERR_UNDECLARED_INT_CONSTANT,currentToken->lineNo, currentToken->colNo);
    break;
  default:
    error(ERR_INVALID_CONSTANT, lookAhead->lineNo, lookAhead->colNo);
    break;
  }
  return constValue;
}

Type* compileType(void) {
  Type* type;
  Type* elementType;
  int arraySize;
  Object* obj;

  switch (lookAhead->tokenType) {
  case KW_INTEGER: 
    eat(KW_INTEGER);
    type =  makeIntType();
    break;
  case KW_CHAR: 
    eat(KW_CHAR); 
    type = makeCharType();
    break;
  case KW_STRING:
    eat(KW_STRING);
    type = makeStringType();
    break;
  case KW_DOUBLE:
    eat(KW_DOUBLE);
    type = makeDoubleType();
    break;
  case KW_ARRAY:
    eat(KW_ARRAY);
    eat(SB_LSEL);
    eat(TK_NUMBER);

    arraySize = currentToken->value;

    eat(SB_RSEL);
    eat(KW_OF);
    elementType = compileType();
    type = makeArrayType(arraySize, elementType);
    break;
  case TK_IDENT:
    eat(TK_IDENT);
    obj = checkDeclaredType(currentToken->string);
    type = duplicateType(obj->typeAttrs->actualType);
    break;
  default:
    error(ERR_INVALID_TYPE, lookAhead->lineNo, lookAhead->colNo);
    break;
  }
  return type;
}

Type* compileBasicType(void) {
  Type* type;

  switch (lookAhead->tokenType) {
  case KW_INTEGER: 
    eat(KW_INTEGER); 
    type = makeIntType();
    break;
  case KW_CHAR: 
    eat(KW_CHAR); 
    type = makeCharType();
    break;
  default:
    error(ERR_INVALID_BASICTYPE, lookAhead->lineNo, lookAhead->colNo);
    break;
  }
  return type;
}

void compileParams(void) {
  if (lookAhead->tokenType == SB_LPAR) {
    eat(SB_LPAR);
    compileParam();
    while (lookAhead->tokenType == SB_SEMICOLON) {
      eat(SB_SEMICOLON);
      compileParam();
    }
    eat(SB_RPAR);
  }
}

void compileParam(void) {
  Object* param;
  Type* type;
  enum ParamKind paramKind = PARAM_VALUE;

  if (lookAhead->tokenType == KW_VAR) {
    paramKind = PARAM_REFERENCE;
    eat(KW_VAR);
  }

  eat(TK_IDENT);
  checkFreshIdent(currentToken->string);
  param = createParameterObject(currentToken->string, paramKind);
  eat(SB_COLON);
  type = compileBasicType();
  param->paramAttrs->type = type;
  declareObject(param);
}

void compileStatements(void) {
  compileStatement();
  while (lookAhead->tokenType == SB_SEMICOLON) {
    eat(SB_SEMICOLON);
    compileStatement();
  }
}

void compileStatement(void) {
  switch (lookAhead->tokenType) {
  case TK_IDENT:
    compileAssignSt();
    break;
  case KW_CALL:
    compileCallSt();
    break;
  case KW_BEGIN:
    compileGroupSt();
    break;
  case KW_IF:
    compileIfSt();
    break;
  case KW_WHILE:
    compileWhileSt();
    break;
  case KW_DO:
    compileDoWhileSt();
    break;
  case KW_REPEAT:
    compileRepeatSt();
    break;
  case KW_FOR:
    eat(KW_FOR);
    if(lookAhead->tokenType == SB_LPAR) compileCFor();
    else compileForSt();
    break;
    // EmptySt needs to check FOLLOW tokens
  case SB_SEMICOLON:
  case KW_END:
  case KW_ELSE:
    break;
    // Error occurs
  default:
    error(ERR_INVALID_STATEMENT, lookAhead->lineNo, lookAhead->colNo);
    break;
  }
}

Type* compileLValue(void) {
  Object* var;
  Type* varType;

  eat(TK_IDENT);
  
  var = checkDeclaredLValueIdent(currentToken->string);

  switch (var->kind) {
  case OBJ_VARIABLE:
    genVariableAddress(var);

    if (var->varAttrs->type->typeClass == TP_ARRAY) {
      varType = compileIndexes(var->varAttrs->type);
    }
    else
      varType = var->varAttrs->type;
    break;
  case OBJ_PARAMETER:
    // TODO: push parameter value onto stack if the parameter is a reference (defined with keyword VAR)
    //       push parameter address onto stack if the parameter is a value
    switch(var->paramAttrs->kind){
      case PARAM_VALUE:
        genParameterAddress(var); // ??? WHY
        break;
      case PARAM_REFERENCE:
        genParameterValue(var); // Why
        break;
    }
    varType = var->paramAttrs->type;
    break;
  case OBJ_FUNCTION:
    // TODO: push the return value address onto the stack
    genReturnValueAddress(var);
    varType = var->funcAttrs->returnType;
    break;
  default: 
    error(ERR_INVALID_LVALUE,currentToken->lineNo, currentToken->colNo);
  }

  return varType;
}

Type* compileLValueIdent(char *str){
  Object* var;
  Type* varType;
  
  var = checkDeclaredLValueIdent(str);

  switch (var->kind) {
  case OBJ_VARIABLE:
    genVariableAddress(var);

    if (var->varAttrs->type->typeClass == TP_ARRAY) {
      varType = compileIndexes(var->varAttrs->type);
    }
    else
      varType = var->varAttrs->type;
    break;
  case OBJ_PARAMETER:
    // TODO: push parameter value onto stack if the parameter is a reference (defined with keyword VAR)
    //       push parameter address onto stack if the parameter is a value
    switch(var->paramAttrs->kind){
      case PARAM_VALUE:
        genParameterAddress(var); // ??? WHY
        break;
      case PARAM_REFERENCE:
        genParameterValue(var); // Why
        break;
    }
    varType = var->paramAttrs->type;
    break;
  case OBJ_FUNCTION:
    // TODO: push the return value address onto the stack
    genReturnValueAddress(var);
    varType = var->funcAttrs->returnType;
    break;
  default: 
    error(ERR_INVALID_LVALUE,currentToken->lineNo, currentToken->colNo);
  }

  return varType;
}

void compileAssignSt(void) {
  Type* varType;
  Type* expType;
  LValueList *List, *node, *curNode;
  List = (LValueList*)malloc(sizeof(LValueList));
  List->next = NULL;
  strcpy(List->ident, lookAhead->string);

  varType = compileLValue();
  curNode = List;
  while(lookAhead->tokenType == SB_COMMA){
    eat(SB_COMMA);
    node = (LValueList*)malloc(sizeof(LValueList));
    node->next = NULL;
    strcpy(node->ident, lookAhead->string);
    curNode->next = node;
    curNode = node;
    eat(TK_IDENT);
  }

  eat(SB_ASSIGN);
  expType = compileExpression();
  checkTypeEquality(varType, expType);
  genST();
  curNode = List->next;
  while(lookAhead->tokenType == SB_COMMA){
    eat(SB_COMMA);
    if(curNode == NULL) error(ERR_LVALUE_INCONSISTENCY, currentToken->lineNo, currentToken->colNo);
    varType = compileLValueIdent(curNode->ident);
    expType = compileExpression();
    checkTypeEquality(varType, expType);
    genST();
    curNode = curNode->next;
  }
  if(curNode) error(ERR_LVALUE_INCONSISTENCY, currentToken->lineNo, currentToken->colNo);
}

void compileCallSt(void) {
  // TODO: generate call-statement
  Object* proc;

  eat(KW_CALL);
  eat(TK_IDENT);

  proc = checkDeclaredProcedure(currentToken->string);

  
  if (isPredefinedProcedure(proc)) {
    compileArguments(proc->procAttrs->paramList);
    genPredefinedProcedureCall(proc);
  } else {
    genProcedureCall(proc); 
  }
}

void compileGroupSt(void) {
  eat(KW_BEGIN);
  compileStatements();
  eat(KW_END);
}

void compileIfSt(void) {
  Instruction* fjInstruction;
  Instruction* jInstruction;

  eat(KW_IF);
  compileCondition();
  eat(KW_THEN);

  fjInstruction = genFJ(DC_VALUE);
  compileStatement();
  if (lookAhead->tokenType == KW_ELSE) {
    jInstruction = genJ(DC_VALUE);
    updateFJ(fjInstruction, getCurrentCodeAddress());
    eat(KW_ELSE);
    compileStatement();
    updateJ(jInstruction, getCurrentCodeAddress());
  } else {
    updateFJ(fjInstruction, getCurrentCodeAddress());
  }
}

void compileWhileSt(void) {
  CodeAddress beginWhile;
  Instruction* fjInstruction;

  beginWhile = getCurrentCodeAddress();
  eat(KW_WHILE);
  compileCondition();
  fjInstruction = genFJ(DC_VALUE);
  eat(KW_DO);
  compileStatement();
  genJ(beginWhile);
  updateFJ(fjInstruction, getCurrentCodeAddress());
}
void compileDoWhileSt(){
  CodeAddress beginDoWhile;

  beginDoWhile = getCurrentCodeAddress();
  eat(KW_DO);
  compileStatement();
  eat(KW_WHILE);
  compileCondition();
  genFJ(getCurrentCodeAddress() + 2);
  genJ(beginDoWhile);
}

void compileRepeatSt(void){
  CodeAddress begin;
  eat(KW_REPEAT);
  begin = getCurrentCodeAddress();
  compileStatement();
  eat(KW_UNTIL);
  compileCondition();
  genFJ(begin);
}

void compileForSt(void) {
  CodeAddress beginLoop;
  Instruction* fjInstruction;
  Type* varType;
  Type *type;

  // eat(KW_FOR);

  varType = compileLValue();
  eat(SB_ASSIGN);

  genCV();
  type = compileExpression();
  checkTypeEquality(varType, type);
  genST();
  genCV();
  genLI();
  beginLoop = getCurrentCodeAddress();
  eat(KW_TO);

  type = compileExpression();
  checkTypeEquality(varType, type);
  genLE();
  fjInstruction = genFJ(DC_VALUE);

  eat(KW_DO);
  compileStatement();

  genCV();  
  genCV();
  genLI();
  genLC(1);
  genAD();
  genST();

  genCV();
  genLI();

  genJ(beginLoop);
  updateFJ(fjInstruction, getCurrentCodeAddress());
  genDCT(1);

}

void compileCFor(void){
  // eat(KW_FOR);
  Instruction *fjInstruction, *jInstruction;
  CodeAddress L1, L2, L3, L4;

  eat(SB_LPAR);
  compileAssignSt();
  eat(SB_SEMICOLON);
  L1 = getCurrentCodeAddress();
  compileCondition();
  fjInstruction = genFJ(0);
  jInstruction = genJ(0);

  eat(SB_SEMICOLON);
  L2 = getCurrentCodeAddress();
  compileAssignSt();
  genJ(L1);

  eat(SB_RPAR);
  L3 = getCurrentCodeAddress();
  compileStatement();
  genJ(L2);
  L4 = getCurrentCodeAddress();

  updateFJ(fjInstruction, L4);
  updateJ(jInstruction, L3);
}

void compileArgument(Object* param) {
  Type* type;

  if (param->paramAttrs->kind == PARAM_VALUE) {
    type = compileExpression();
    checkTypeEquality(type, param->paramAttrs->type);
  } else {
    type = compileLValue();
    checkTypeEquality(type, param->paramAttrs->type);
  }
}

void compileArguments(ObjectNode* paramList) {
  ObjectNode* node = paramList;

  switch (lookAhead->tokenType) {
  case SB_LPAR:
    eat(SB_LPAR);
    if (node == NULL)
      error(ERR_PARAMETERS_ARGUMENTS_INCONSISTENCY, currentToken->lineNo, currentToken->colNo);
    compileArgument(node->object);
    node = node->next;

    while (lookAhead->tokenType == SB_COMMA) {
      eat(SB_COMMA);
      if (node == NULL)
	error(ERR_PARAMETERS_ARGUMENTS_INCONSISTENCY, currentToken->lineNo, currentToken->colNo);
      compileArgument(node->object);
      node = node->next;
    }

    if (node != NULL)
      error(ERR_PARAMETERS_ARGUMENTS_INCONSISTENCY, currentToken->lineNo, currentToken->colNo);
    
    eat(SB_RPAR);
    break;
    // Check FOLLOW set 
  case SB_TIMES:
  case SB_SLASH:
  case SB_PLUS:
  case SB_MINUS:
  case KW_TO:
  case KW_DO:
  case SB_RPAR:
  case SB_COMMA:
  case SB_EQ:
  case SB_NEQ:
  case SB_LE:
  case SB_LT:
  case SB_GE:
  case SB_GT:
  case SB_RSEL:
  case SB_SEMICOLON:
  case KW_END:
  case KW_ELSE:
  case KW_THEN:
    break;
  default:
    error(ERR_INVALID_ARGUMENTS, lookAhead->lineNo, lookAhead->colNo);
  }
}

void compileCondition(void) {
  Type* type1;
  Type* type2;
  TokenType op;

  type1 = compileExpression();
  checkBasicType(type1);

  op = lookAhead->tokenType;
  switch (op) {
  case SB_EQ:
    eat(SB_EQ);
    break;
  case SB_NEQ:
    eat(SB_NEQ);
    break;
  case SB_LE:
    eat(SB_LE);
    break;
  case SB_LT:
    eat(SB_LT);
    break;
  case SB_GE:
    eat(SB_GE);
    break;
  case SB_GT:
    eat(SB_GT);
    break;
  default:
    error(ERR_INVALID_COMPARATOR, lookAhead->lineNo, lookAhead->colNo);
  }

  type2 = compileExpression();
  checkTypeEquality(type1,type2);

  switch (op) {
  case SB_EQ:
    genEQ();
    break;
  case SB_NEQ:
    genNE();
    break;
  case SB_LE:
    genLE();
    break;
  case SB_LT:
    genLT();
    break;
  case SB_GE:
    genGE();
    break;
  case SB_GT:
    genGT();
    break;
  default:
    break;
  }

}

Type* compileExpression(void) {
  Type* type;
  Instruction* fjInstruction, *jInstruction;

  switch (lookAhead->tokenType) {
  case SB_PLUS:
    eat(SB_PLUS);
    type = compileExpression2();
    checkNumberType(type);
    break;
  case SB_MINUS:
    eat(SB_MINUS);
    type = compileExpression2();
    checkNumberType(type);
    genNEG();
    break;
  case KW_IF:
    eat(KW_IF);
    compileCondition();
    fjInstruction = genFJ(0);
    eat(KW_RETURN);
    type = compileExpression();
    jInstruction = genJ(0);
    eat(KW_ELSE);
    eat(KW_RETURN);
    updateFJ(fjInstruction, getCurrentCodeAddress());
    type = compileExpression();
    updateJ(jInstruction, getCurrentCodeAddress());
    break;
  default:
    type = compileExpression2();
  }
  return type;
}

Type* compileExpression2(void) {
  Type* type;

  type = compileTerm();
  type = compileExpression3(type);

  return type;
}


Type* compileExpression3(Type* argType1) {
  Type* argType2;
  Type* resultType;

  switch (lookAhead->tokenType) {
  case SB_PLUS:
    eat(SB_PLUS);
    checkNumberType(argType1);
    argType2 = compileTerm();
    checkNumberType(argType2);

    genAD();

    resultType = compileExpression3(argType1);
    break;
  case SB_MINUS:
    eat(SB_MINUS);
    checkNumberType(argType1);
    argType2 = compileTerm();
    checkNumberType(argType2);

    genSB();

    resultType = compileExpression3(argType1);
    break;
    // check the FOLLOW set
  case KW_TO:
  case KW_DO:
  case SB_RPAR:
  case SB_COMMA:
  case SB_EQ:
  case SB_NEQ:
  case SB_LE:
  case SB_LT:
  case SB_GE:
  case SB_GT:
  case SB_RSEL:
  case SB_SEMICOLON:
  case KW_END:
  case KW_ELSE:
  case KW_THEN:
  case KW_WHILE:
  case KW_RETURN:
  case KW_UNTIL:
    resultType = argType1;
    break;
  default:
    error(ERR_INVALID_EXPRESSION, lookAhead->lineNo, lookAhead->colNo);
  }
  return resultType;
}

Type* compileTerm(void) {
  Type* type;
  type = compileFactor();
  type = compileTerm2(type);

  return type;
}

Type* compileTerm2(Type* argType1) {
  Type* argType2;
  Type* resultType;

  switch (lookAhead->tokenType) {
  case SB_TIMES:
    eat(SB_TIMES);
    checkIntType(argType1);
    argType2 = compileFactor();
    checkIntType(argType2);

    genML();

    resultType = compileTerm2(argType1);
    break;
  case SB_SLASH:
    eat(SB_SLASH);
    checkIntType(argType1);
    argType2 = compileFactor();
    checkIntType(argType2);

    genDV();

    resultType = compileTerm2(argType1);
    break;
    // check the FOLLOW set
  case SB_PLUS:
  case SB_MINUS:
  case KW_TO:
  case KW_DO:
  case SB_RPAR:
  case SB_COMMA:
  case SB_EQ:
  case SB_NEQ:
  case SB_LE:
  case SB_LT:
  case SB_GE:
  case SB_GT:
  case SB_RSEL:
  case SB_SEMICOLON:
  case KW_END:
  case KW_ELSE:
  case KW_THEN:
  case KW_WHILE:
  case KW_RETURN:
  case KW_UNTIL:
    resultType = argType1;
    break;
  default:
    error(ERR_INVALID_TERM, lookAhead->lineNo, lookAhead->colNo);
  }
  return resultType;
}

Type* compileFactor(void) {
  Type* type;
  Object* obj;

  switch (lookAhead->tokenType) {
  case TK_NUMBER:
    eat(TK_NUMBER);
    type = intType;
    genLC(currentToken->value);
    break;
  case TK_FLOAT:
    eat(TK_FLOAT);
    type = doubleType;
    genLC(currentToken->fValue);
    break;
  case TK_CHAR:
    eat(TK_CHAR);
    type = charType;
    genLC(currentToken->value);
    break;
  case TK_IDENT:
    eat(TK_IDENT);
    obj = checkDeclaredIdent(currentToken->string);

    switch (obj->kind) {
    case OBJ_CONSTANT:
      switch (obj->constAttrs->value->type) {
          case TP_INT:
        	 type = intType;
        	 genLC(obj->constAttrs->value->intValue);
        	break;
              case TP_CHAR:
        	type = charType;
        	genLC(obj->constAttrs->value->charValue);
        	break;
              default:
        	break;
      }
      break;
    case OBJ_VARIABLE:
      if (obj->varAttrs->type->typeClass == TP_ARRAY) {
      	genVariableAddress(obj);
      	type = compileIndexes(obj->varAttrs->type);
      	genLI();
            } else {
      	type = obj->varAttrs->type;
      	genVariableValue(obj);
            }
      break;
    case OBJ_PARAMETER:
      // TODO: push parameter's value onto the stack
      genParameterValue(obj);
      type = obj->paramAttrs->type;
      break;
    case OBJ_FUNCTION:
            // TODO: generate function call
            if (isPredefinedFunction(obj)) {
            	compileArguments(obj->funcAttrs->paramList);
            	genPredefinedFunctionCall(obj);
            } else {
                genFunctionCall(obj);
            }
            type = obj->funcAttrs->returnType;
      break;
    default: 
        error(ERR_INVALID_FACTOR,currentToken->lineNo, currentToken->colNo);
        break;
    }
    break;
  case SB_LPAR:
    eat(SB_LPAR);
    type = compileExpression();
    eat(SB_RPAR);
    break;
  default:
    error(ERR_INVALID_FACTOR, lookAhead->lineNo, lookAhead->colNo);
  }
  
  return type;
}

Type* compileIndexes(Type* arrayType) {
  // TODO: Generate code for computing array element address
  Type* type;
  Type *loop;
  int res = 1;
  while (lookAhead->tokenType == SB_LSEL) {
    eat(SB_LSEL);
    type = compileExpression();
    checkIntType(type);
    checkArrayType(arrayType);
    arrayType = arrayType->elementType;
    eat(SB_RSEL);
    loop = arrayType;
    res = 1;
    while(loop->elementType) {
      res *= loop->arraySize;
      loop = loop->elementType;
    }
    genLC(res);
    genML();
    genAD();
  }
  checkBasicType(arrayType);
  return arrayType;
}

int compile(char *fileName) {
  if (openInputStream(fileName) == IO_ERROR)
    return IO_ERROR;

  currentToken = NULL;
  lookAhead = getValidToken();

  initSymTab();

  compileProgram();

  cleanSymTab();
  free(currentToken);
  free(lookAhead);
  closeInputStream();
  return IO_SUCCESS;

}
