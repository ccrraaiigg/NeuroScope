# HookManager Class Documentation

## Purpose

HookManager serves as the central orchestrator for the NeuroScope hook system, managing the lifecycle, execution, and coordination of all hooks attached to a transformer model. It provides a unified interface for hook registration, execution scheduling, and resource management while ensuring optimal performance during forward passes.

## Responsibilities

- **Hook Registration**: Manage the registration and deregistration of hooks with validation and conflict resolution
- **Execution Orchestration**: Coordinate hook execution during forward passes with proper ordering and dependency management
- **Lifecycle Management**: Handle hook initialization, execution, and cleanup phases with error recovery
- **Performance Optimization**: Minimize overhead during forward passes through efficient hook scheduling and caching
- **Resource Management**: Track memory usage and computational resources consumed by active hooks
- **Error Handling**: Provide robust error handling and recovery mechanisms for hook failures
- **State Coordination**: Maintain consistent state across multiple hooks and execution contexts

## Key Concepts

The HookManager implements a sophisticated execution model that balances flexibility with performance. It maintains separate execution contexts for different types of hooks (monitoring vs. intervention) and provides dependency resolution for complex hook interactions.

The manager operates in two primary modes: **registration mode** (when hooks are being added/removed) and **execution mode** (during forward passes). This separation allows for optimization strategies that minimize runtime overhead while maintaining full flexibility during setup.

Hook execution follows a carefully orchestrated pipeline that respects dependencies, handles errors gracefully, and provides detailed execution metrics for performance analysis and debugging.

## Instance Variables

- **activeHooks**: A dictionary mapping layer identifiers to collections of registered hooks. Organized by layer for efficient lookup during forward pass execution. Keys are layer indices or symbolic identifiers (#embedding, #all), values are ordered collections of Hook instances.

- **model**: Reference to the TransformerModel instance this manager is attached to. Used for accessing model state, configuration, and coordinating with the forward pass execution pipeline.

- **executionContext**: Current execution state including active forward pass information, hook execution stack, and performance metrics. Maintains context needed for proper hook coordination and error recovery.

- **hookDependencies**: Dependency graph tracking inter-hook relationships and execution ordering constraints. Used to resolve execution order and detect circular dependencies during registration.

- **performanceMetrics**: Collection of execution statistics including hook execution times, memory usage, and error counts. Provides detailed profiling information for optimization and debugging.

- **errorHandlers**: Registry of error handling strategies for different types of hook failures. Allows customization of error recovery behavior and logging strategies.

## Usage Patterns

### Basic Hook Management
```smalltalk
"Create and configure hook manager"
manager := HookManager for: model.

"Register simple monitoring hook"
monitorHook := ActivationHook new
    name: 'layer_monitor';
    layer: 5;
    action: [:activation | self logActivation: activation];
    yourself.

manager addHook: monitorHook.

"Register intervention hook"
interventionHook := InterventionHook new
    name: 'attention_ablation';
    layer: 8;
    action: [:activation | activation zeroHeads: #(3 7)];
    yourself.

manager addHook: interventionHook.

"Execute forward pass with hooks"
tokens := model tokenizer encode: 'Hello world'.
output := model forward: tokens. "Hooks execute automatically"
```

### Advanced Hook Registration
```smalltalk
"Register multiple hooks with dependencies"
preprocessHook := Hook new
    name: 'preprocessing';
    layer: 6;
    action: [:activation | activation normalize];
    yourself.

analysisHook := Hook new
    name: 'analysis';
    layer: 6;
    dependsOn: #('preprocessing');
    action: [:activation | self analyzeActivation: activation];
    yourself.

manager 
    addHook: preprocessHook;
    addHook: analysisHook.

"Batch registration with validation"
hookCollection := {monitorHook. interventionHook. analysisHook}.
manager addHooks: hookCollection validateDependencies: true.

"Conditional registration"
debugMode ifTrue: [
    debugHooks := self createDebugHooks.
    manager addHooks: debugHooks.
].
```

### Hook Execution Control
```smalltalk
"Enable/disable hooks dynamically"
manager disableHook: 'attention_ablation'.
manager enableHook: 'attention_ablation'.

"Temporary hook suspension"
manager withHooksSuspended: #('layer_monitor') do: [
    fastOutput := model forward: tokens.
].

"Selective hook execution"
manager executeOnlyHooks: #('preprocessing' 'analysis') during: [
    analysisOutput := model forward: tokens.
].

"Hook execution with custom context"
customContext := HookExecutionContext new
    maxExecutionTime: 1000;
    memoryLimit: 100 megabytes;
    errorStrategy: #continue;
    yourself.

manager executeWithContext: customContext during: [
    output := model forward: tokens.
].
```

## Integration Points

### With TransformerModel
The HookManager integrates deeply with the model's forward pass execution, receiving callbacks at key computation points and coordinating hook execution with minimal performance impact.

### With Layer Classes
Individual layer classes provide hook execution points and coordinate with the manager to ensure hooks receive appropriate activation tensors and execution context.

### With Hook Instances
The manager maintains bidirectional communication with hook instances, providing execution context and receiving results, errors, and performance metrics.

### With ActivationTensor
Hook execution operates primarily on ActivationTensor instances, with the manager ensuring proper tensor lifecycle management and memory optimization.

### With Analysis Tools
Analysis classes often interact with the HookManager to register temporary hooks for data collection and coordinate complex multi-hook analysis workflows.

## Examples

### Complete Hook Management Workflow
```smalltalk
"Initialize hook manager for model"
model := TransformerModel fromHuggingFace: 'gpt2-small'.
manager := model hookManager.

"Create comprehensive analysis setup"
activationCache := ActivationHook new
    name: 'activation_cache';
    layer: #all;
    action: [:activation :layerIndex | 
        self cacheActivation: activation at: layerIndex.
        activation
    ];
    yourself.

attentionAnalyzer := ActivationHook new
    name: 'attention_analysis';
    layer: #attention;
    dependsOn: #('activation_cache');
    action: [:activation | 
        patterns := self extractAttentionPatterns: activation.
        self recordPatterns: patterns.
        activation
    ];
    yourself.

interventionHook := InterventionHook new
    name: 'controlled_intervention';
    layer: 8;
    condition: [:activation | interventionActive];
    action: [:activation | 
        interventionActive 
            ifTrue: [activation * interventionStrength]
            ifFalse: [activation]
    ];
    yourself.

"Register hooks with dependency resolution"
manager 
    addHook: activationCache;
    addHook: attentionAnalyzer;
    addHook: interventionHook;
    validateDependencies.

"Execute analysis with performance monitoring"
manager enablePerformanceMonitoring.

tokens := model tokenizer encode: 'The quick brown fox jumps over the lazy dog'.
startTime := Time millisecondClockValue.
output := model forward: tokens.
endTime := Time millisecondClockValue.

"Access results and metrics"
cachedActivations := activationCache results.
attentionPatterns := attentionAnalyzer results.
executionMetrics := manager performanceMetrics.

Transcript show: 'Total execution time: ', (endTime - startTime) asString, 'ms'.
Transcript show: 'Hook overhead: ', executionMetrics hookOverhead asString, 'ms'.
```

### Error Handling and Recovery
```smalltalk
"Configure robust error handling"
manager errorHandlingStrategy: #continueOnError.

"Register error-prone hook with recovery"
experimentalHook := Hook new
    name: 'experimental_analysis';
    layer: 7;
    action: [:activation | 
        [self performExperimentalAnalysis: activation]
            on: Error
            do: [:error | 
                manager recordError: error for: self.
                self fallbackAnalysis: activation
            ]
    ];
    onTimeout: 2000 do: [
        manager recordTimeout: self.
        nil "Skip analysis on timeout"
    ];
    yourself.

manager addHook: experimentalHook.

"Custom error handler"
manager onHookError: [:hook :error | 
    Transcript show: 'Hook ', hook name, ' failed: ', error messageText.
    hook isEssential 
        ifTrue: [error signal] "Re-raise for essential hooks"
        ifFalse: [self logError: error] "Log and continue for optional hooks"
].

"Execute with error monitoring"
[output := model forward: tokens]
    ensure: [
        errors := manager collectedErrors.
        errors notEmpty ifTrue: [
            self reportErrors: errors.
        ].
    ].
```

### Performance Optimization
```smalltalk
"Configure performance optimization"
manager 
    enableHookCaching: true;
    setExecutionBudget: 100 milliseconds;
    optimizeForThroughput.

"Batch hook execution for efficiency"
batchHooks := manager createBatchExecutor
    maxBatchSize: 10;
    timeoutPerBatch: 50 milliseconds;
    yourself.

"Profile hook performance"
profiler := HookProfiler new.
manager installProfiler: profiler.

"Execute with profiling"
100 timesRepeat: [
    output := model forward: tokens.
].

"Analyze performance results"
profile := profiler generateReport.
slowHooks := profile hooksSlowerThan: 10 milliseconds.
memoryHogs := profile hooksUsingMoreThan: 10 megabytes.

"Optimize based on profiling"
slowHooks do: [:hook |
    manager optimizeHook: hook.
].

memoryHogs do: [:hook |
    manager enableMemoryOptimization: hook.
].
```

### Dynamic Hook Management
```smalltalk
"Runtime hook modification"
manager onModelStateChange: [:newState |
    newState isTraining 
        ifTrue: [
            manager enableHooks: trainingHooks.
            manager disableHooks: inferenceHooks.
        ]
        ifFalse: [
            manager enableHooks: inferenceHooks.
            manager disableHooks: trainingHooks.
        ].
].

"Adaptive hook scheduling"
adaptiveScheduler := AdaptiveHookScheduler new
    performanceTarget: 50 milliseconds;
    qualityThreshold: 0.95;
    yourself.

manager installScheduler: adaptiveScheduler.

"Hook hot-swapping"
manager replaceHook: 'old_analysis' with: improvedAnalysisHook.

"Conditional hook activation"
manager addConditionalHook: seasonalHook 
    condition: [:context | Date today month = 12].

"Load balancing across hooks"
manager enableLoadBalancing
    strategy: #roundRobin
    maxConcurrentHooks: 5.
```

### Integration with Analysis Workflows
```smalltalk
"Coordinate complex analysis pipeline"
analysisWorkflow := AnalysisWorkflow new.

"Phase 1: Data collection"
collectionHooks := analysisWorkflow createCollectionHooks.
manager addHooks: collectionHooks.

tokens := model tokenizer encode: analysisText.
model forward: tokens. "Collect data"

"Phase 2: Analysis"
manager removeHooks: collectionHooks.
analysisHooks := analysisWorkflow createAnalysisHooks: collectionHooks results.
manager addHooks: analysisHooks.

model forward: tokens. "Perform analysis"

"Phase 3: Intervention"
manager removeHooks: analysisHooks.
interventionHooks := analysisWorkflow createInterventionHooks: analysisHooks results.
manager addHooks: interventionHooks.

interventionResults := model forward: tokens. "Test interventions"

"Cleanup and report"
manager removeAllHooks.
report := analysisWorkflow generateReport: {
    collectionHooks results.
    analysisHooks results.
    interventionResults.
}.
```

### Hook Composition and Coordination
```smalltalk
"Create coordinated hook ensemble"
ensemble := HookEnsemble new
    name: 'circuit_analysis_ensemble';
    coordinator: CircuitAnalysisCoordinator new;
    yourself.

"Add component hooks"
ensemble 
    addHook: (self createActivationExtractor: 'layer_5')
    addHook: (self createAttentionAnalyzer: 'layer_8')
    addHook: (self createInterventionTester: 'layer_11')
    addHook: (self createResultAggregator: 'final').

"Register ensemble as single unit"
manager addEnsemble: ensemble.

"Execute coordinated analysis"
circuitResults := ensemble executeAnalysis: tokens.

"Access coordinated results"
activations := ensemble results at: 'activations'.
attentionPatterns := ensemble results at: 'attention_patterns'.
interventionEffects := ensemble results at: 'intervention_effects'.
aggregatedInsights := ensemble results at: 'insights'.
```

This comprehensive documentation establishes HookManager as the central coordination point for all hook-based functionality in NeuroScope, providing developers with the tools and patterns needed to implement sophisticated interpretability analyses with optimal performance and robust error handling.#
# Method Documentation

### Class Methods

#### for: model
**Purpose**: Creates a new HookManager instance for the specified TransformerModel.

**Parameters**:
- `model` (TransformerModel): The model this manager will coordinate hooks for

**Return Value**: A new HookManager instance configured for the model

**Usage Examples**:
```smalltalk
"Create manager for model"
model := TransformerModel fromHuggingFace: 'gpt2-small'.
manager := HookManager for: model.

"Create with custom configuration"
manager := (HookManager for: model)
    maxConcurrentHooks: 10;
    enablePerformanceMonitoring: true;
    yourself.
```

#### new
**Purpose**: Creates a new HookManager instance without an associated model.

**Return Value**: A new HookManager instance that must be configured with a model

**Usage Examples**:
```smalltalk
"Create manager and configure later"
manager := HookManager new.
manager model: myModel.
```

### Instance Methods

#### addHook: hook
**Purpose**: Registers a hook with the manager for execution during forward passes.

**Parameters**:
- `hook` (Hook): The hook instance to register

**Return Value**: The registered hook (for method chaining)

**Side Effects**: 
- Adds hook to the active hooks registry
- Validates hook configuration
- Resolves dependencies if present

**Usage Examples**:
```smalltalk
"Register single hook"
manager addHook: monitoringHook.

"Method chaining"
manager 
    addHook: preprocessingHook;
    addHook: analysisHook;
    addHook: postprocessingHook.

"Conditional registration"
debugMode ifTrue: [
    manager addHook: debugHook
].
```

**Error Conditions**:
- Raises `DuplicateHookError` if hook name already exists
- Raises `InvalidHookError` if hook configuration is invalid
- Raises `DependencyError` if hook dependencies cannot be resolved

#### removeHook: hookOrName
**Purpose**: Unregisters a hook from the manager.

**Parameters**:
- `hookOrName` (Hook or String): Hook instance or name to remove

**Return Value**: Boolean indicating whether the hook was found and removed

**Side Effects**: 
- Removes hook from active registry
- Updates dependency graph
- Cleans up hook resources

**Usage Examples**:
```smalltalk
"Remove by hook instance"
success := manager removeHook: myHook.

"Remove by name"
manager removeHook: 'attention_monitor'.

"Conditional removal"
experimentEnded ifTrue: [
    manager removeHook: 'experimental_hook'
].

"Remove multiple hooks"
#('hook1' 'hook2' 'hook3') do: [:name |
    manager removeHook: name
].
```

#### removeAllHooks
**Purpose**: Removes all registered hooks from the manager.

**Return Value**: Self (for method chaining)

**Side Effects**: 
- Clears all hook registrations
- Resets dependency graph
- Releases all hook resources

**Usage Examples**:
```smalltalk
"Clean slate"
manager removeAllHooks.

"Reset between experiments"
manager 
    removeAllHooks;
    addHooks: newExperimentHooks.

"Cleanup before model disposal"
[self disposeModel]
    ensure: [manager removeAllHooks].
```

#### addHooks: hookCollection
**Purpose**: Registers multiple hooks in a single operation.

**Parameters**:
- `hookCollection` (Collection of Hook): Hooks to register

**Return Value**: Self (for method chaining)

**Side Effects**: 
- Registers all hooks in the collection
- Resolves dependencies across the entire set
- Validates the complete hook configuration

**Usage Examples**:
```smalltalk
"Register hook collection"
analysisHooks := {
    preprocessingHook.
    analysisHook.
    postprocessingHook.
}.
manager addHooks: analysisHooks.

"Register with validation"
manager addHooks: experimentHooks validateDependencies: true.
```

#### hookNamed: hookName
**Purpose**: Retrieves a registered hook by its name.

**Parameters**:
- `hookName` (String): Name of the hook to retrieve

**Return Value**: The Hook instance, or nil if not found

**Usage Examples**:
```smalltalk
"Access specific hook"
analysisHook := manager hookNamed: 'attention_analysis'.

"Check hook existence"
(manager hookNamed: 'optional_hook') ifNotNil: [:hook |
    hook activate
].

"Modify existing hook"
monitorHook := manager hookNamed: 'monitor'.
monitorHook condition: [:activation | newCondition].
```

#### hooksForLayer: layerIdentifier
**Purpose**: Returns all hooks registered for the specified layer.

**Parameters**:
- `layerIdentifier` (Integer or Symbol): Layer index or identifier

**Return Value**: Collection of Hook instances for the layer

**Usage Examples**:
```smalltalk
"Get layer-specific hooks"
layer5Hooks := manager hooksForLayer: 5.

"Check layer hook count"
hookCount := (manager hooksForLayer: 8) size.
hookCount > 5 ifTrue: [
    Transcript show: 'Warning: Many hooks on layer 8'
].

"Disable all hooks for a layer"
(manager hooksForLayer: 6) do: [:hook |
    hook deactivate
].
```

#### allHooks
**Purpose**: Returns all registered hooks across all layers.

**Return Value**: Collection of all Hook instances

**Usage Examples**:
```smalltalk
"Get all hooks"
allHooks := manager allHooks.

"Count total hooks"
totalHooks := manager allHooks size.

"Find hooks by criteria"
slowHooks := manager allHooks select: [:hook |
    hook averageExecutionTime > 10
].

"Reset all hooks"
manager allHooks do: [:hook | hook reset].
```

#### executeHooksForLayer: layerIndex activation: activation
**Purpose**: Executes all registered hooks for the specified layer with the given activation.

**Parameters**:
- `layerIndex` (Integer): Layer index where execution occurs
- `activation` (ActivationTensor): Activation tensor to process

**Return Value**: The potentially modified activation tensor

**Side Effects**: 
- Executes all applicable hooks in dependency order
- Updates hook execution statistics
- May modify the activation tensor

**Usage Examples**:
```smalltalk
"Manual hook execution (typically called by model)"
modifiedActivation := manager 
    executeHooksForLayer: 5 
    activation: layerActivation.

"Execute with error handling"
[result := manager executeHooksForLayer: layerIndex activation: activation]
    on: HookExecutionError
    do: [:error | 
        self handleHookError: error.
        activation "Return original on error"
    ].
```

**Performance Notes**: Execution order is optimized based on dependencies and performance characteristics.

#### enableHook: hookName
**Purpose**: Activates a specific hook for execution.

**Parameters**:
- `hookName` (String): Name of the hook to enable

**Return Value**: Boolean indicating success

**Usage Examples**:
```smalltalk
"Enable specific hook"
manager enableHook: 'attention_analysis'.

"Conditional enabling"
analysisMode ifTrue: [
    manager enableHook: 'detailed_analysis'
].

"Enable multiple hooks"
#('hook1' 'hook2' 'hook3') do: [:name |
    manager enableHook: name
].
```

#### disableHook: hookName
**Purpose**: Deactivates a specific hook to prevent execution.

**Parameters**:
- `hookName` (String): Name of the hook to disable

**Return Value**: Boolean indicating success

**Usage Examples**:
```smalltalk
"Disable expensive hook"
manager disableHook: 'expensive_analysis'.

"Temporary disabling"
manager disableHook: 'intervention_hook'.
[self runCleanAnalysis]
    ensure: [manager enableHook: 'intervention_hook'].
```

#### enableAllHooks
**Purpose**: Activates all registered hooks for execution.

**Return Value**: Self (for method chaining)

**Usage Examples**:
```smalltalk
"Enable all hooks"
manager enableAllHooks.

"Reset and enable"
manager 
    removeAllHooks;
    addHooks: newHooks;
    enableAllHooks.
```

#### disableAllHooks
**Purpose**: Deactivates all registered hooks to prevent execution.

**Return Value**: Self (for method chaining)

**Usage Examples**:
```smalltalk
"Disable all hooks for fast execution"
manager disableAllHooks.
fastResult := model forward: tokens.
manager enableAllHooks.

"Temporary suspension"
manager disableAllHooks.
[self performCriticalOperation]
    ensure: [manager enableAllHooks].
```

#### withHooksSuspended: hookNames do: block
**Purpose**: Temporarily disables specified hooks during block execution.

**Parameters**:
- `hookNames` (Array of String): Names of hooks to suspend
- `block` (Block): Code to execute with hooks suspended

**Return Value**: The result of executing the block

**Side Effects**: 
- Temporarily disables specified hooks
- Restores hook states after block execution

**Usage Examples**:
```smalltalk
"Suspend expensive hooks for fast execution"
fastResult := manager 
    withHooksSuspended: #('expensive_analysis' 'detailed_monitor')
    do: [model forward: tokens].

"Suspend intervention hooks for clean analysis"
cleanResult := manager
    withHooksSuspended: #('attention_ablation' 'activation_patching')
    do: [model forward: tokens].
```

#### executeOnlyHooks: hookNames during: block
**Purpose**: Executes only the specified hooks during block execution.

**Parameters**:
- `hookNames` (Array of String): Names of hooks to keep active
- `block` (Block): Code to execute with limited hooks

**Return Value**: The result of executing the block

**Usage Examples**:
```smalltalk
"Execute only essential hooks"
result := manager
    executeOnlyHooks: #('error_detection' 'safety_monitor')
    during: [model forward: dangerousTokens].

"Focused analysis"
analysisResult := manager
    executeOnlyHooks: #('data_collection' 'pattern_analysis')
    during: [model forward: analysisTokens].
```

#### validateDependencies
**Purpose**: Validates that all hook dependencies can be resolved.

**Return Value**: Boolean indicating whether all dependencies are valid

**Side Effects**: May reorder hooks to satisfy dependencies

**Usage Examples**:
```smalltalk
"Validate after adding hooks"
manager addHooks: complexHooks.
manager validateDependencies ifFalse: [
    self error: 'Hook dependency validation failed'
].

"Check before execution"
manager validateDependencies ifTrue: [
    result := model forward: tokens
].
```

**Error Conditions**:
- Raises `CircularDependencyError` if circular dependencies are detected
- Raises `MissingDependencyError` if required dependencies are not registered

#### resolveDependencies
**Purpose**: Automatically resolves and orders hooks based on their dependencies.

**Return Value**: Self (for method chaining)

**Side Effects**: Reorders hooks to satisfy dependency constraints

**Usage Examples**:
```smalltalk
"Auto-resolve dependencies"
manager 
    addHooks: unorderedHooks;
    resolveDependencies.

"Resolve after modifications"
manager 
    removeHook: 'old_hook';
    addHook: newHook;
    resolveDependencies.
```

#### enablePerformanceMonitoring
**Purpose**: Enables detailed performance tracking for hook execution.

**Return Value**: Self (for method chaining)

**Side Effects**: Begins collecting detailed execution metrics

**Usage Examples**:
```smalltalk
"Enable monitoring"
manager enablePerformanceMonitoring.

"Monitor during analysis"
manager enablePerformanceMonitoring.
result := model forward: tokens.
metrics := manager performanceMetrics.
```

#### disablePerformanceMonitoring
**Purpose**: Disables performance tracking to reduce overhead.

**Return Value**: Self (for method chaining)

**Usage Examples**:
```smalltalk
"Disable for production"
manager disablePerformanceMonitoring.

"Reduce overhead during intensive processing"
manager disablePerformanceMonitoring.
results := self runLargeExperiment.
```

#### performanceMetrics
**Purpose**: Returns detailed performance statistics for hook execution.

**Return Value**: Dictionary containing execution metrics

**Usage Examples**:
```smalltalk
"Get performance data"
metrics := manager performanceMetrics.
totalTime := metrics at: #totalExecutionTime.
hookOverhead := metrics at: #hookOverhead.

"Analyze hook performance"
metrics := manager performanceMetrics.
slowHooks := metrics at: #slowHooks.
memoryUsage := metrics at: #memoryUsage.

"Performance reporting"
metrics := manager performanceMetrics.
self generatePerformanceReport: metrics.
```

#### clearPerformanceMetrics
**Purpose**: Resets all collected performance statistics.

**Return Value**: Self (for method chaining)

**Usage Examples**:
```smalltalk
"Reset metrics between experiments"
manager clearPerformanceMetrics.

"Clean slate for new analysis"
manager 
    clearPerformanceMetrics;
    enablePerformanceMonitoring.
```

#### setExecutionTimeout: milliseconds
**Purpose**: Sets a timeout limit for hook execution to prevent hanging.

**Parameters**:
- `milliseconds` (Integer): Maximum execution time per hook

**Return Value**: Self (for method chaining)

**Side Effects**: Hooks exceeding timeout will be terminated

**Usage Examples**:
```smalltalk
"Set reasonable timeout"
manager setExecutionTimeout: 1000. "1 second"

"Strict timeout for production"
manager setExecutionTimeout: 100. "100ms"

"Disable timeout"
manager setExecutionTimeout: nil.
```

#### onHookError: errorBlock
**Purpose**: Sets a global error handler for hook execution failures.

**Parameters**:
- `errorBlock` (Block): Block that receives hook and error, returns recovery action

**Return Value**: Self (for method chaining)

**Usage Examples**:
```smalltalk
"Global error handling"
manager onHookError: [:hook :error |
    Transcript show: 'Hook ', hook name, ' failed: ', error messageText.
    self logHookError: hook error: error.
    #continue "Continue with other hooks"
].

"Strict error handling"
manager onHookError: [:hook :error |
    hook isEssential 
        ifTrue: [error signal] "Re-raise for essential hooks"
        ifFalse: [#skip] "Skip non-essential hooks"
].
```

#### optimizeForThroughput
**Purpose**: Configures the manager for maximum execution speed.

**Return Value**: Self (for method chaining)

**Side Effects**: 
- Reduces monitoring overhead
- Optimizes hook execution order
- Enables performance optimizations

**Usage Examples**:
```smalltalk
"Optimize for production"
manager optimizeForThroughput.

"High-performance mode"
manager 
    optimizeForThroughput;
    disablePerformanceMonitoring;
    setExecutionTimeout: 50.
```

#### optimizeForDebugging
**Purpose**: Configures the manager for maximum debugging information.

**Return Value**: Self (for method chaining)

**Side Effects**: 
- Enables detailed monitoring
- Adds safety checks
- Provides verbose error reporting

**Usage Examples**:
```smalltalk
"Debug mode"
manager optimizeForDebugging.

"Development configuration"
manager 
    optimizeForDebugging;
    enablePerformanceMonitoring;
    setExecutionTimeout: nil.
```

#### cleanup
**Purpose**: Releases all resources and prepares the manager for disposal.

**Return Value**: Self

**Side Effects**: 
- Removes all hooks
- Clears performance metrics
- Releases memory resources

**Usage Examples**:
```smalltalk
"Cleanup before disposal"
manager cleanup.

"Reset manager state"
manager 
    cleanup;
    addHooks: newHooks.

"Ensure cleanup"
[self useManager: manager]
    ensure: [manager cleanup].
```