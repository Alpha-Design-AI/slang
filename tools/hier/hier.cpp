//------------------------------------------------------------------------------
//! @file hier.cpp
//! @brief A tool for printing information about a Verilog hierarchy
//
// SPDX-FileCopyrightText: Michael Popoloski
// SPDX-License-Identifier: MIT
//------------------------------------------------------------------------------
#include <algorithm>
#include <regex>
#include <fstream>

#include "slang/ast/ASTVisitor.h"
#include "slang/ast/Compilation.h"
#include "slang/ast/symbols/InstanceSymbols.h"
#include "slang/ast/symbols/CompilationUnitSymbols.h"
#include "slang/ast/symbols/PortSymbols.h"
#include "slang/ast/SemanticFacts.h"
#include "slang/syntax/AllSyntax.h"
#include "slang/driver/Driver.h"
#include "slang/util/VersionInfo.h"
#include "slang/util/String.h"

using namespace slang;
using namespace slang::driver;
using namespace slang::ast;

struct PortInfo {
    std::string name;
    std::string direction;
    std::string type;
};

struct InstanceInfo {
    std::string type;        // "module" or "interface"
    std::string module;
    std::string instance;
    std::string file;
    size_t line;
    size_t column;
    std::string definition_file;
    size_t definition_line;
    size_t definition_column;
    std::map<std::string, std::string> parameters;
    std::vector<PortInfo> ports;
};

struct DefinitionInfo {
    std::string name;
    std::string type;        // "module" or "interface"
    std::string file;
    size_t line;
    size_t column;
    std::vector<PortInfo> ports;
};

int main(int argc, char** argv) {
    std::regex regex;
    std::smatch match;
    Driver driver;
    driver.addStandardArgs();

    std::optional<bool> showHelp;
    std::optional<bool> showVersion;
    std::optional<bool> params;
    std::optional<bool> ports;
    std::optional<bool> diagnostics;
    std::optional<bool> definitions;
    std::optional<bool> location;
    std::optional<int> maxDepth;
    std::optional<std::string> instPrefix;
    std::optional<std::string> instRegex;
    std::optional<std::string> customFormat;
    std::optional<std::string> jsonOutput;
    driver.cmdLine.add("-h,--help", showHelp, "Display available options");
    driver.cmdLine.add("--version", showVersion, "Display version information and exit");
    driver.cmdLine.add("--params", params, "Display instance parameter values");
    driver.cmdLine.add("--ports", ports, "Display instance port information");
    driver.cmdLine.add("--diagnostics", diagnostics, "Display compilation diagnostics and warnings");
    driver.cmdLine.add("--definitions", definitions, "Display module definitions with source file information");
    driver.cmdLine.add("--location", location, "Display source location (line:column) in output");
    driver.cmdLine.add("--json", jsonOutput, "Output hierarchy in JSON format to specified file, or '-' for stdout", "<file>");
    driver.cmdLine.add("--max-depth", maxDepth, "Maximum instance depth to be printed", "<depth>");
    driver.cmdLine.add("--inst-prefix", instPrefix,
                       "Skip all instance subtrees not under this prefix (inst.sub_inst...)",
                       "<inst-prefix>");
    driver.cmdLine.add("--inst-regex", instRegex,
                       "Show only instances matched by regex (scans whole tree)", "<inst-regex>");
    driver.cmdLine.add(
        "--custom-format", customFormat,
        "Use libfmt-style strings to format output with {inst}, {module}, {file}, {line}, {column} as argument names",
        "<fmt::format string>");

    if (!driver.parseCommandLine(argc, argv))
        return 1;

    if (showHelp == true) {
        printf("%s\n", driver.cmdLine.getHelpText("slang SystemVerilog compiler").c_str());
        return 0;
    }

    if (showVersion == true) {
        printf("slang version %d.%d.%d+%s\n", VersionInfo::getMajor(), VersionInfo::getMinor(),
               VersionInfo::getPatch(), std::string(VersionInfo::getHash()).c_str());
        return 0;
    }

    if (!driver.processOptions())
        return 2;

    bool ok = driver.parseAllSources();

    auto compilation = driver.createCompilation();
    auto sourceManager = compilation->getSourceManager();

    if (instRegex.has_value())
        regex = instRegex.value();
    
    std::vector<InstanceInfo> instanceList;
    std::vector<DefinitionInfo> definitionList;
    
    if (!jsonOutput.has_value())
        OS::print("--- Instances ---\n");
    
    auto instances = compilation->getRoot().topInstances;
    for (auto& i : instances) {
        int depth = maxDepth.value_or(-1); // will never be 0, go full depth
        int pathLength = instPrefix.value_or("").length();
        int index = 0;
        i->visit(makeVisitor([&](auto& visitor, const InstanceSymbol& type) {
            if (type.isModule()) {
                int len = type.name.length();
                int save_index = index;
                // if no instPrefix, pathLength is 0, and this check will never take place, so
                // instPrefix.value() is safe if index >= pathLength we satisfied the full
                // instPrefix. from now on we are limited only by max-depth
                if (index < pathLength) {
                    if (type.name !=
                        instPrefix.value().substr(index, std::min(pathLength - index, len))) {
                        // current instance name did not match
                        return;
                    }
                    index += len;
                    if (index < pathLength && instPrefix.value()[index] != '.')
                        return; // separator needed, but didn't find one
                    index++;    // adjust for '.'
                }

                auto s_inst = type.getHierarchicalPath();
                if (!instRegex.has_value() || std::regex_search(s_inst, match, regex)) {
                    auto s_module = type.getDefinition().name;
                    
                    // Instance location (where it's instantiated)
                    auto instLocation = type.location;
                    auto s_file = sourceManager->getFileName(instLocation);
                    auto lineNum = sourceManager->getLineNumber(instLocation);
                    auto colNum = sourceManager->getColumnNumber(instLocation);
                    
                    // Definition location (where the module is defined)
                    auto defLocation = type.getDefinition().location;
                    auto s_def_file = sourceManager->getFileName(defLocation);
                    auto defLineNum = sourceManager->getLineNumber(defLocation);
                    auto defColNum = sourceManager->getColumnNumber(defLocation);
                    
                    // Determine type (module or interface)
                    std::string instType = type.isInterface() ? "interface" : "module";
                    
                    // Collect parameters
                    std::map<std::string, std::string> paramMap;
                    if (params.value_or(false)) {
                        for (auto p : type.body.getParameters()) {
                            std::string v;
                            if (p->symbol.kind == SymbolKind::Parameter)
                                v = p->symbol.as<ParameterSymbol>().getValue().toString();
                            else if (p->symbol.kind == SymbolKind::TypeParameter)
                                v = p->symbol.as<TypeParameterSymbol>()
                                        .targetType.getType()
                                        .toString();
                            else
                                v = "?";
                            paramMap[std::string(p->symbol.name)] = v;
                        }
                    }
                    
                    // Collect port connections
                    std::vector<PortInfo> portInfos;
                    if (ports.value_or(false)) {
                        for (auto conn : type.getPortConnections()) {
                            if (!conn) continue;
                            
                            std::string portName;
                            std::string direction;
                            std::string connection;
                            
                            if (conn->port.kind == SymbolKind::Port) {
                                auto& portSym = conn->port.as<PortSymbol>();
                                portName = std::string(portSym.name);
                                switch (portSym.direction) {
                                    case ArgumentDirection::In: direction = "input"; break;
                                    case ArgumentDirection::Out: direction = "output"; break;
                                    case ArgumentDirection::InOut: direction = "inout"; break;
                                    case ArgumentDirection::Ref: direction = "ref"; break;
                                }
                                
                                // Get the connection expression
                                auto expr = conn->getExpression();
                                if (expr && expr->syntax) {
                                    connection = expr->syntax->toString();
                                } else if (expr && expr->sourceRange.start().buffer()) {
                                    // Try to extract text from source range using SourceManager
                                    auto range = expr->sourceRange;
                                    auto startOffset = range.start().offset();
                                    auto endOffset = range.end().offset();
                                    auto buffer = sourceManager->getSourceText(range.start().buffer());
                                    if (startOffset < buffer.length() && endOffset <= buffer.length()) {
                                        connection = std::string(buffer.substr(startOffset, endOffset - startOffset));
                                    } else {
                                        connection = "<expression>";
                                    }
                                } else if (expr) {
                                    connection = "<expression>";
                                } else {
                                    connection = "<unconnected>";
                                }
                            } else if (conn->port.kind == SymbolKind::InterfacePort) {
                                auto& ifacePort = conn->port.as<InterfacePortSymbol>();
                                portName = std::string(ifacePort.name);
                                direction = "interface";
                                
                                auto expr = conn->getExpression();
                                if (expr && expr->syntax) {
                                    connection = expr->syntax->toString();
                                } else if (expr && expr->sourceRange.start().buffer()) {
                                    // Try to extract text from source range using SourceManager
                                    auto range = expr->sourceRange;
                                    auto startOffset = range.start().offset();
                                    auto endOffset = range.end().offset();
                                    auto buffer = sourceManager->getSourceText(range.start().buffer());
                                    if (startOffset < buffer.length() && endOffset <= buffer.length()) {
                                        connection = std::string(buffer.substr(startOffset, endOffset - startOffset));
                                    } else {
                                        connection = "<expression>";
                                    }
                                } else if (expr) {
                                    connection = "<expression>";
                                } else {
                                    connection = "<unconnected>";
                                }
                            } else {
                                continue;
                            }
                            
                            portInfos.push_back({portName, direction, connection});
                        }
                    }
                    
                    // Store for JSON output if needed
                    if (jsonOutput.has_value()) {
                        instanceList.push_back({instType, std::string(s_module), s_inst, std::string(s_file),
                                               lineNum, colNum, std::string(s_def_file), defLineNum, defColNum,
                                               paramMap, portInfos});
                    } else {
                        // Text output
                        std::string fileStr = std::string(s_file);
                        if (location.value_or(false)) {
                            fileStr += fmt::format(":{}:{}", lineNum, colNum);
                        }
                        
                        if (customFormat.has_value())
                            OS::print(fmt::format(fmt::runtime(customFormat.value()),
                                                  fmt::arg("module", s_module),
                                                  fmt::arg("inst", s_inst),
                                                  fmt::arg("file", fileStr),
                                                  fmt::arg("line", lineNum),
                                                  fmt::arg("column", colNum)));
                        else
                            OS::print(fmt::format("Type=\"{}\" Module=\"{}\" Instance=\"{}\" File=\"{}\" ",
                                                  instType, s_module, s_inst, fileStr));
                        
                        if (!paramMap.empty()) {
                            OS::print(fmt::format("Parameters: "));
                            int size = paramMap.size();
                            for (const auto& [name, value] : paramMap) {
                                size--;
                                OS::print(fmt::format("{}={}{}", name, value, size ? ", " : ""));
                            }
                        }
                        
                        if (!portInfos.empty()) {
                            if (!paramMap.empty())
                                OS::print(" ");
                            OS::print(fmt::format("Connections: "));
                            for (size_t i = 0; i < portInfos.size(); i++) {
                                const auto& port = portInfos[i];
                                OS::print(fmt::format(".{}({})", port.name, port.type));
                                if (i < portInfos.size() - 1)
                                    OS::print(", ");
                            }
                        }
                        OS::print("\n");
                    }
                }
                depth--;
                if (depth)
                    visitor.visitDefault(type);
                depth++;
                index = save_index;
            }
        }));
    }

    // Collect definitions if requested (must be done before JSON output)
    if (definitions.value_or(false)) {
        for (auto def : compilation->getDefinitions()) {
            // Check if this is a DefinitionSymbol (module or interface, skip programs)
            if (def->kind == SymbolKind::Definition) {
                auto& defSym = def->as<DefinitionSymbol>();
                
                // Skip programs
                if (defSym.definitionKind == DefinitionKind::Program)
                    continue;
                
                auto s_name = defSym.name;
                auto defLocation = defSym.location;
                auto s_file = sourceManager->getFileName(defLocation);
                auto lineNum = sourceManager->getLineNumber(defLocation);
                auto colNum = sourceManager->getColumnNumber(defLocation);
                auto s_type = defSym.definitionKind == DefinitionKind::Module ? "module" : "interface";
                
                // Collect port definitions if --ports is specified
                std::vector<PortInfo> defPortInfos;
                if (ports.value_or(false)) {
                    // Create a default instance to get the elaborated port list
                    try {
                        auto& defaultInst = InstanceSymbol::createDefault(*compilation, defSym);
                        for (auto port : defaultInst.body.getPortList()) {
                            if (port->kind == SymbolKind::Port) {
                                auto& portSym = port->as<PortSymbol>();
                                std::string dir;
                                switch (portSym.direction) {
                                    case ArgumentDirection::In: dir = "input"; break;
                                    case ArgumentDirection::Out: dir = "output"; break;
                                    case ArgumentDirection::InOut: dir = "inout"; break;
                                    case ArgumentDirection::Ref: dir = "ref"; break;
                                }
                                defPortInfos.push_back({
                                    std::string(portSym.name),
                                    dir,
                                    portSym.getType().toString()
                                });
                            } else if (port->kind == SymbolKind::InterfacePort) {
                                auto& ifacePort = port->as<InterfacePortSymbol>();
                                defPortInfos.push_back({
                                    std::string(ifacePort.name),
                                    "interface",
                                    ifacePort.interfaceDef ? std::string(ifacePort.interfaceDef->name) : "generic"
                                });
                            }
                        }
                    } catch (...) {
                        // If we can't create a default instance (e.g., missing parameters), skip ports
                    }
                }
                
                definitionList.push_back({std::string(s_name), s_type,
                                         std::string(s_file), lineNum, colNum, defPortInfos});
            }
        }
    }

    // Output JSON if requested
    if (jsonOutput.has_value()) {
        // Collect top-level module names
        std::vector<std::string> topLevelModules;
        for (auto& inst : compilation->getRoot().topInstances) {
            topLevelModules.push_back(std::string(inst->name));
        }
        
        std::string jsonStr = "{\n";
        jsonStr += "  \"top_modules\": [\n";
        for (size_t i = 0; i < topLevelModules.size(); i++) {
            jsonStr += fmt::format("    \"{}\"", topLevelModules[i]);
            if (i < topLevelModules.size() - 1)
                jsonStr += ",";
            jsonStr += "\n";
        }
        jsonStr += "  ],\n";
        
        jsonStr += "  \"instances\": [\n";
        for (size_t i = 0; i < instanceList.size(); i++) {
            const auto& inst = instanceList[i];
            jsonStr += "    {\n";
            jsonStr += fmt::format("      \"type\": \"{}\",\n", inst.type);
            jsonStr += fmt::format("      \"module\": \"{}\",\n", inst.module);
            jsonStr += fmt::format("      \"instance\": \"{}\",\n", inst.instance);
            jsonStr += fmt::format("      \"file\": \"{}\"", inst.file);
            
            if (location.value_or(false)) {
                jsonStr += ",\n";
                jsonStr += fmt::format("      \"line\": {},\n", inst.line);
                jsonStr += fmt::format("      \"column\": {},\n", inst.column);
                jsonStr += fmt::format("      \"definition_file\": \"{}\",\n", inst.definition_file);
                jsonStr += fmt::format("      \"definition_line\": {},\n", inst.definition_line);
                jsonStr += fmt::format("      \"definition_column\": {}", inst.definition_column);
            } else {
                jsonStr += ",\n";
                jsonStr += fmt::format("      \"definition_file\": \"{}\"", inst.definition_file);
            }
            
            if (!inst.parameters.empty()) {
                jsonStr += ",\n      \"parameters\": {\n";
                size_t paramCount = inst.parameters.size();
                size_t paramIdx = 0;
                for (const auto& [name, value] : inst.parameters) {
                    jsonStr += fmt::format("        \"{}\": \"{}\"", name, value);
                    if (++paramIdx < paramCount)
                        jsonStr += ",";
                    jsonStr += "\n";
                }
                jsonStr += "      }";
            }
            
            if (!inst.ports.empty()) {
                jsonStr += ",\n      \"connections\": [\n";
                for (size_t j = 0; j < inst.ports.size(); j++) {
                    const auto& port = inst.ports[j];
                    jsonStr += "        {\n";
                    jsonStr += fmt::format("          \"port\": \"{}\",\n", port.name);
                    jsonStr += fmt::format("          \"direction\": \"{}\",\n", port.direction);
                    jsonStr += fmt::format("          \"connection\": \"{}\"\n", port.type);
                    jsonStr += "        }";
                    if (j < inst.ports.size() - 1)
                        jsonStr += ",";
                    jsonStr += "\n";
                }
                jsonStr += "      ]";
            }
            
            jsonStr += "\n    }";
            if (i < instanceList.size() - 1)
                jsonStr += ",";
            jsonStr += "\n";
        }
        jsonStr += "  ]";
        
        // Add definitions if requested
        if (definitions.value_or(false) && !definitionList.empty()) {
            jsonStr += ",\n  \"definitions\": [\n";
            for (size_t i = 0; i < definitionList.size(); i++) {
                const auto& def = definitionList[i];
                jsonStr += "    {\n";
                jsonStr += fmt::format("      \"name\": \"{}\",\n", def.name);
                jsonStr += fmt::format("      \"type\": \"{}\",\n", def.type);
                jsonStr += fmt::format("      \"file\": \"{}\"", def.file);
                
                if (location.value_or(false)) {
                    jsonStr += ",\n";
                    jsonStr += fmt::format("      \"line\": {},\n", def.line);
                    jsonStr += fmt::format("      \"column\": {}", def.column);
                }
                
                if (!def.ports.empty()) {
                    jsonStr += ",\n      \"ports\": [\n";
                    for (size_t j = 0; j < def.ports.size(); j++) {
                        const auto& port = def.ports[j];
                        jsonStr += "        {\n";
                        jsonStr += fmt::format("          \"name\": \"{}\",\n", port.name);
                        jsonStr += fmt::format("          \"direction\": \"{}\",\n", port.direction);
                        jsonStr += fmt::format("          \"type\": \"{}\"\n", port.type);
                        jsonStr += "        }";
                        if (j < def.ports.size() - 1)
                            jsonStr += ",";
                        jsonStr += "\n";
                    }
                    jsonStr += "      ]";
                }
                
                jsonStr += "\n    }";
                if (i < definitionList.size() - 1)
                    jsonStr += ",";
                jsonStr += "\n";
            }
            jsonStr += "  ]";
        }
        
        jsonStr += "\n}\n";
        
        if (jsonOutput.value() == "-") {
            OS::print(jsonStr);
        } else {
            std::ofstream outFile(jsonOutput.value());
            if (outFile.is_open()) {
                outFile << jsonStr;
                outFile.close();
            } else {
                OS::print(fmt::format("Error: Could not open file '{}' for writing\n", jsonOutput.value()));
                return 4;
            }
        }
    }

    // Output definitions for text mode
    if (definitions.value_or(false) && !jsonOutput.has_value()) {
        OS::print("\n--- Definitions ---\n");
        for (const auto& def : definitionList) {
            std::string fileStr = def.file;
            if (location.value_or(false)) {
                fileStr += fmt::format(":{}:{}", def.line, def.column);
            }
            
            if (customFormat.has_value())
                OS::print(fmt::format(fmt::runtime(customFormat.value()),
                                      fmt::arg("module", def.name),
                                      fmt::arg("inst", def.name), 
                                      fmt::arg("file", fileStr),
                                      fmt::arg("line", def.line),
                                      fmt::arg("column", def.column)));
            else
                OS::print(fmt::format("Definition=\"{}\" Type=\"{}\" File=\"{}\" ",
                                      def.name, def.type, fileStr));
            
            if (!def.ports.empty()) {
                OS::print(fmt::format("Ports: "));
                for (size_t i = 0; i < def.ports.size(); i++) {
                    const auto& port = def.ports[i];
                    OS::print(fmt::format("{} {} {}", port.direction, port.type, port.name));
                    if (i < def.ports.size() - 1)
                        OS::print(", ");
                }
            }
            OS::print("\n");
        }
    }

    if (!jsonOutput.has_value() && diagnostics.value_or(false)) {
        driver.reportCompilation(*compilation, /* quiet */ false);
        ok &= driver.reportDiagnostics(/* quiet */ false);
    }

    return ok ? 0 : 3;
}
