/*
MAIN.CPP
by Nesta Lenhert-Scholer and Bowen Bao
for MPCS 51300 - Compilers

This is the main driving file for the Extended-Kaleidoscope Language.
*/

// C Headers
#include <getopt.h>
#include <fcntl.h>

// C++ Headers
#include <iostream>
#include <string>
#include <fstream>
#include <cstdio>
#include "ast.hpp"
#include "cint_check.hpp"


// TODO: Add more verbosity

// Usage statement to be printed when [-h|-?] options are passed in, 
// or when an invalid option is passed in
static std::string USAGE = 
R"(usage: ./bin/ekcc [-h|-?] [-v] [-emit-ast] [-emit-llvm] [-jit] -o <output_file> <input_file>

Extended-Kaleidoscope Compiler for MPCS 51300 - Compilers
by Nesta Lenhert-Scholer and Bowen Bao

Commands:
-h | -?             Print this help message
-v                  Turn on verbose mode
-O                  Turn on optimizations
-emit-ast           Output the abstract syntax tree and exit
-emit-llvm          Output the LLVM and exit
-jit                Execute the program using JIT Compilation.
-o <output_file>    Write output to file specified by <output_file>. Default is standard out.
<input_file>        Source code file to compile)";

// Variables for compiler flags
static bool verbose;
static int emit_ast;
static int emit_llvm;
static int optimize;
static int jit;

// Stuff from flex/bison that main needs
extern Prog* the_prog;
extern int yyparse();
extern int yylineno;
extern int yydebug;
extern void yyset_debug(int);
extern FILE* yyin;
std::string infile;
std::vector<std::string> g_argv;

// Initialize BaseNode static variables
bool BaseNode::has_entry = false;
bool BaseNode::last_scope_function = false;
std::map<std::string, std::pair<Type*, Decls*>> BaseNode::functions;
std::vector<llvm::Function*> BaseNode::llvmFunctions;
std::vector<std::map<std::string, Type*>> BaseNode::scopes;
std::vector<std::map<std::string, llvm::AllocaInst*>> BaseNode::inScopeVars;
std::unique_ptr<llvm::LLVMContext> BaseNode::Context;
std::unique_ptr<llvm::Module> BaseNode::Module;
std::unique_ptr<llvm::IRBuilder<>> BaseNode::Builder;

void print_llvm(std::string outfile, llvm::Module* M = nullptr) {
    // Print all the ir
        std::error_code ec;
        llvm::raw_fd_ostream file(outfile, ec);

        if(outfile != "-") {
            if(!M) {
                BaseNode::Module->print(file, nullptr);
            } else {
                M->print(file, nullptr);
            }
        } else {
            if(!M) {
                BaseNode::Module->print(llvm::outs(), nullptr);
            } else {
                M->print(llvm::outs(), nullptr);
            }
        }
}

// Implement the arg and argf functions
extern "C" int arg(int i) {
    if(i < g_argv.size()) {
        try {
            return std::stoi(g_argv[i]);
        } catch (std::out_of_range& e) {
            std::cout << "error: Unable to convert positional argument " << g_argv[i] 
                      << " to an integer" << std::endl;
            exit(1);
        }
    } else {
        std::cout << "error: Index " << i << " out of range for arg function" << std::endl;
        exit(1);
    }
} 

extern "C" float argf(int i) {
    if(i < g_argv.size()) {
        try {
            return std::stof(g_argv[i]);
        } catch (std::out_of_range& e) {
            std::cout << "error: Unable to convert positional argument " << g_argv[i] 
                      << " to a float" << std::endl;
            exit(1);
        }
    } else {
        std::cout << "error: Index " << i << " out of range for argf function" << std::endl;
        exit(1);
    }
} 

int main(int argc, char* argv[]) {

    if(argc == 1) {
        std::cerr << USAGE << std::endl;
        return 1;
    }

    llvm::InitLLVM X(argc, argv);

    // Initialize the target registry etc.
    llvm::InitializeNativeTarget();
    llvm::InitializeAllAsmParsers();
    llvm::InitializeAllAsmPrinters();

    auto TargetTriple = llvm::sys::getDefaultTargetTriple();
    std::string CPU = llvm::sys::getHostCPUName();

    llvm::StringMap<bool> HostFeatures;
    std::string FeaturesStr;
    if (llvm::sys::getHostCPUFeatures(HostFeatures)) {
        llvm::SubtargetFeatures Features;

        for (auto &F : HostFeatures)
            Features.AddFeature(F.first(), F.second);

        FeaturesStr = Features.getString();
  }


    yyset_debug(0);                     // Set debug for flex

    std::string outfile = "-";      // Default output file

    // Options for getopt_long
    static option long_options[] = { 
        {"emit-ast", no_argument, &emit_ast, 1},
        {"emit-llvm", no_argument, &emit_llvm, 1},
        {"jit", no_argument, &jit, 1},
        {0, 0, 0, 0}
    };

    // loop to get command line arguments
    while(true) {
        int option_idx = 0;
        int c = getopt_long_only(argc, argv, "h?Ovo:", long_options, &option_idx);

        if(c == -1) {
            break;
        }

        switch(c) {
            case 'v':
                verbose = true;
                break;
            case 'o':
                outfile = optarg;
                break;
            case 'O':
                optimize = true;
                break;
            case 'h':
            case '?':
                std::cerr << USAGE << std::endl;
                return 1;
        }
    }

    // Get the input file name
    if(optind < argc) {
        infile = argv[optind++];
        if(verbose) {
            std::cout << "Input Source Code File Name: " << infile
                      << "\nOutput File Name: " << outfile << std::endl;
        }
    } else {
        std::cerr << "error: No input file provided" << std::endl; 
        return 1;
    }

    // Add positional arguments for arg and argf functions
    while(optind < argc) {
        g_argv.push_back(argv[optind++]);
    }

    // Exit if no commands were added
    if(!emit_ast && !emit_llvm && !jit) {
        return 0;
    }

    // Open input file
    std::FILE* fin = fopen(infile.c_str(), "r");
    if(!fin) {
        std::cerr << "error: Unable to open input file: " << infile << std::endl;
        return 1;
    }

    yyin = fin;

    // Set debug information
    if(verbose) {
        yydebug = 1;
        yyset_debug(1);
    }

    // Parse the file
    while(!feof(yyin)) {
        yyparse();
    }

    fclose(fin);

    // Error occured while parsing
    if(!the_prog) {
        std::cerr << "error: Program was not able to correctly parse text file" << std::endl;
        return 1;
    } else if(!BaseNode::has_entry) {
        std::cerr << "error: No \"run\" function defined." << std::endl;
        return 1;
    }

    // Emit AST and exit
    if(emit_ast) {
        std::ostream* out = &std::cout;
        std::ofstream fout;
        if(outfile != "-") {
            fout.open(outfile);
            if(!fout.is_open()) {
                std::cerr << "error: Unable to open output stream: " << outfile << std::endl;
                return 1;
            }
            out = &fout;
        }
        
        *out << "---\n";
        the_prog->print_yaml(*out, "");
        *out << "...\n";
        if(fout.is_open()) fout.close();
        return 0;
    }

    // Generate LLVM IR for the program as well as TM
    the_prog->codegen();
    BaseNode::Module->setTargetTriple(llvm::StringRef(TargetTriple));
    llvm::verifyModule(*BaseNode::Module);
    std::string Err;
    const llvm::Target* Target = llvm::TargetRegistry::lookupTarget(TargetTriple, Err);
    std::unique_ptr<llvm::TargetMachine> TM(Target->createTargetMachine(TargetTriple, CPU, 
                                                                        FeaturesStr, 
                                                                        llvm::TargetOptions(), 
                                                                        llvm::Reloc::PIC_));
    BaseNode::Module->setDataLayout(TM->createDataLayout());

    // Emit unoptimized LLVM and exit
    if(!optimize && emit_llvm) {
        print_llvm(outfile);
        return 0;
    }

    if(optimize) {
        llvm::Triple ModuleTriple(BaseNode::Module->getTargetTriple());
        std::unique_ptr<llvm::legacy::PassManager> MPM(new llvm::legacy::PassManager);
        llvm::TargetLibraryInfoImpl TLII(ModuleTriple);
        MPM->add(new llvm::TargetLibraryInfoWrapperPass(TLII));

        std::unique_ptr<llvm::legacy::FunctionPassManager> 
            FPM(new llvm::legacy::FunctionPassManager(BaseNode::Module.get()));
        FPM->add(llvm::createTargetTransformInfoWrapperPass(TM->getTargetIRAnalysis()));

        llvm::PassManagerBuilder PMBuilder;
        PMBuilder.OptLevel = 3;
        PMBuilder.SizeLevel = 0;
        PMBuilder.Inliner = llvm::createFunctionInliningPass(PMBuilder.OptLevel, 
                                                             PMBuilder.SizeLevel, 
                                                             false);
        PMBuilder.LoopVectorize = true;

        TM->adjustPassManager(PMBuilder);

        PMBuilder.populateFunctionPassManager(*FPM);
        PMBuilder.populateModulePassManager(*MPM);

        FPM->doInitialization();
        for (llvm::Function &F : *BaseNode::Module)
            FPM->run(F);
        FPM->doFinalization();

        MPM->run(*BaseNode::Module);
    }

    if(emit_llvm) {
        print_llvm(outfile);
        return 0;
    }

    if(jit) {
        // Create JIT Execution Engine
        std::string errStr;
        llvm::ExecutionEngine *executionEngine = llvm::EngineBuilder(std::move(BaseNode::Module))
            .setEngineKind(llvm::EngineKind::JIT)
            .setErrorStr(&errStr)
            .create(TM.get());
        if (!executionEngine) {
            std::cout << "error: " << errStr << std::endl;
            return 1;
        }

        // Add built in internal functions
        executionEngine->addGlobalMapping("arg", (uint64_t)&arg);
        executionEngine->addGlobalMapping("argf", (uint64_t)&argf);
        executionEngine->addGlobalMapping("cint_add", (uint64_t)&cint_add);
        executionEngine->addGlobalMapping("cint_uneg", (uint64_t)&cint_uneg);
        executionEngine->addGlobalMapping("cint_minus", (uint64_t)&cint_minus);
        executionEngine->addGlobalMapping("cint_mult", (uint64_t)&cint_mult);
        executionEngine->addGlobalMapping("cint_div", (uint64_t)&cint_div);



        int (*run)() = (int(*)())executionEngine->getFunctionAddress("run");
        if(!run) {
            std::cout << "error: Unable to execute main function" << std::endl;
            return 1;
        }
        return run();
    }

    // Program shouldn't reach this point
    return 1;
}

void yyerror(const char* s) {
    std::cerr << "error: line " << yylineno << " - " << s << std::endl;
    exit(1);
}