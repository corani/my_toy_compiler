#include "node.h"
#include "codegen.h"
#include "parser.hpp"

using namespace std;

Value *CodeGenBlock::findLocal(std::string name) {
    if (locals.find(name) == locals.end()) {
        return parent ? parent->findLocal(name) : nullptr;
    } else {
        return locals[name];
    }
}

/* Compile the AST into a module */
void CodeGenContext::generateCode(NBlock& root)
{
	std::cout << "Generating code...\n";

	/* Create the top level interpreter function to call as entry */
	vector<Type*> argTypes;
	FunctionType *ftype = FunctionType::get(Type::getVoidTy(getGlobalContext()), makeArrayRef(argTypes), false);
	mainFunction = Function::Create(ftype, GlobalValue::InternalLinkage, "main", module);
	BasicBlock *bblock = BasicBlock::Create(getGlobalContext(), "entry", mainFunction, 0);

	/* Push a new variable/block context */
	pushBlock(bblock);
	root.codeGen(*this); /* emit bytecode for the toplevel block */
	ReturnInst::Create(getGlobalContext(), bblock);
	popBlock();

	/* Print the bytecode in a human-readable format 
	   to see if our program compiled properly
	 */
	std::cout << "Code is generated.\n";
	PassManager pm;
	pm.add(createPrintModulePass(outs()));
	pm.run(*module);
}

void CodeGenContext::pushBlock(BasicBlock *block) {
    CodeGenBlock *child  = new CodeGenBlock();
    child->block = block;
    if (!blocks.empty()) {
        child->parent = blocks.top();
    }
    blocks.push(child);

    builder.SetInsertPoint(blocks.top()->block);
}

void CodeGenContext::popBlock() {
    CodeGenBlock *top = blocks.top();
    blocks.pop();
    delete top;
    if (!blocks.empty()) {
        builder.SetInsertPoint(blocks.top()->block);
    }
}

/* Executes the AST by running the main function */
GenericValue CodeGenContext::runCode() {
	std::cout << "Running code...\n";
	ExecutionEngine *ee = EngineBuilder(module).create();
	vector<GenericValue> noargs;
	GenericValue v = ee->runFunction(mainFunction, noargs);
	std::cout << "Code was run.\n";
	return v;
}

/* Returns an LLVM type based on the identifier */
static Type *typeOf(const NIdentifier& type)
{
	if (type.name.compare("int") == 0) {
		return Type::getInt64Ty(getGlobalContext());
	}
	else if (type.name.compare("double") == 0) {
		return Type::getDoubleTy(getGlobalContext());
	}
    else if (type.name.compare("bool") == 0) {
        return Type::getInt1Ty(getGlobalContext());
    }
    else if (type.name.compare("void") == 0) {
        return Type::getVoidTy(getGlobalContext());
    }
	return Type::getVoidTy(getGlobalContext());
}

/* -- Code Generation -- */

Value* NInteger::codeGen(CodeGenContext& context)
{
	std::cout << "Creating integer: " << value << endl;
	return ConstantInt::get(Type::getInt64Ty(getGlobalContext()), value, true);
}

Value* NDouble::codeGen(CodeGenContext& context)
{
	std::cout << "Creating double: " << value << endl;
	return ConstantFP::get(Type::getDoubleTy(getGlobalContext()), value);
}

Value* NBoolean::codeGen(CodeGenContext& context)
{
    std::cout << "Creating boolean: " << value << endl;
    return ConstantInt::get(Type::getInt1Ty(getGlobalContext()), value);
}

Value* NIdentifier::codeGen(CodeGenContext& context)
{
	std::cout << "Creating identifier reference: " << name << endl;
    Value *var = context.findLocal(name);
	if (!var) {
		std::cerr << "undeclared variable " << name << endl;
		return nullptr;
	}
    return context.builder.CreateLoad(var);
}

Value* NMethodCall::codeGen(CodeGenContext& context)
{
	Function *function = context.module->getFunction(id.name.c_str());
	if (!function) {
		std::cerr << "no such function " << id.name << endl;
	}
	std::vector<Value*> args;
	ExpressionList::const_iterator it;
	for (it = arguments.begin(); it != arguments.end(); it++) {
		args.push_back((**it).codeGen(context));
	}
	std::cout << "Creating method call: " << id.name << endl;
    return context.builder.CreateCall(function, makeArrayRef(args));
}

Value* NBinaryOperator::codeGen(CodeGenContext& context)
{
	std::cout << "Creating binary operation " << op << endl;
    Value* left = lhs.codeGen(context);
    Value* right= rhs.codeGen(context);

	switch (op) {
		case TPLUS:
            return context.builder.CreateAdd(left, right);
		case TMINUS:
            return context.builder.CreateSub(left, right);
		case TMUL:
            return context.builder.CreateMul(left, right);
		case TDIV:
            return context.builder.CreateSDiv(left, right);
        case TCEQ:
            return context.builder.CreateICmpEQ(left, right);
        case TCNE:
            return context.builder.CreateICmpNE(left, right);
        case TCLT:
            return context.builder.CreateICmpSLT(left, right);
        case TCLE:
            return context.builder.CreateICmpSLE(left, right);
        case TCGT:
            return context.builder.CreateICmpSGT(left, right);
        case TCGE:
            return context.builder.CreateICmpSGE(left, right);
        default:
            return nullptr;
	}
}

Value* NAssignment::codeGen(CodeGenContext& context)
{
	std::cout << "Creating assignment for " << lhs.name << endl;
    Value* var = context.findLocal(lhs.name);
	if (var) {
        Value* val = rhs.codeGen(context);
        return context.builder.CreateStore(val, var);
    } else {
        std::cerr << "undeclared variable " << lhs.name << endl;
        return nullptr;
    }
}

Value* NBlock::codeGen(CodeGenContext& context)
{
	StatementList::const_iterator it;
	Value *last = nullptr;
	for (it = statements.begin(); it != statements.end(); it++) {
		std::cout << "Generating code for " << typeid(**it).name() << endl;
		last = (**it).codeGen(context);
	}
	std::cout << "Creating block" << endl;
	return last;
}

Value* NExpressionStatement::codeGen(CodeGenContext& context)
{
	std::cout << "Generating code for " << typeid(expression).name() << endl;
	return expression.codeGen(context);
}

Value* NReturnStatement::codeGen(CodeGenContext& context)
{
	std::cout << "Generating return code for " << typeid(expression).name() << endl;
    if (expression) {
        Value *returnValue = expression->codeGen(context);
	    context.setCurrentReturnValue(returnValue);
        return returnValue;
    } else {
        return nullptr;
    }
}

Value* NVariableDeclaration::codeGen(CodeGenContext& context)
{
	std::cout << "Creating variable declaration " << type.name << " " << id.name << endl;
    Value* var = nullptr;
    if (context.isGlobalContext()) {
        var = new GlobalVariable(*(context.module), typeOf(type), false, GlobalValue::PrivateLinkage,
                Constant::getNullValue(typeOf(type)), id.name.c_str());
    } else {
        var = context.builder.CreateAlloca(typeOf(type));
    }
	context.locals()[id.name] = var;
	if (assignmentExpr) {
		NAssignment assn(id, *assignmentExpr);
		assn.codeGen(context);
	}
	return var;
}

Value* NExternDeclaration::codeGen(CodeGenContext& context)
{
    vector<Type*> argTypes;
    VariableList::const_iterator it;
    for (it = arguments.begin(); it != arguments.end(); it++) {
        argTypes.push_back(typeOf((**it).type));
    }
    FunctionType *ftype = FunctionType::get(typeOf(type), makeArrayRef(argTypes), false);
    Function *function = Function::Create(ftype, GlobalValue::ExternalLinkage, id.name.c_str(), context.module);
    return function;
}

Value* NFunctionDeclaration::codeGen(CodeGenContext& context)
{
	vector<Type*> argTypes;
	VariableList::const_iterator it;
	for (it = arguments.begin(); it != arguments.end(); it++) {
		argTypes.push_back(typeOf((**it).type));
	}
	FunctionType *ftype = FunctionType::get(typeOf(type), makeArrayRef(argTypes), false);
	Function *function = Function::Create(ftype, GlobalValue::InternalLinkage, id.name.c_str(), context.module);
	BasicBlock *bblock = BasicBlock::Create(getGlobalContext(), "entry", function, 0);

	context.pushBlock(bblock);

	Function::arg_iterator argsValues = function->arg_begin();
    Value* argumentValue;

	for (it = arguments.begin(); it != arguments.end(); it++) {
		(**it).codeGen(context);

		argumentValue = argsValues++;
		argumentValue->setName((*it)->id.name.c_str());

        context.builder.CreateStore(argumentValue, context.findLocal((*it)->id.name));
	}

	block.codeGen(context);
    ReturnInst::Create(getGlobalContext(), context.getCurrentReturnValue(), bblock);

	context.popBlock();
	std::cout << "Creating function: " << id.name << endl;
	return function;
}
