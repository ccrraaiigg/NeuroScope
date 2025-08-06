# Hook Base Class Documentation

## Purpose

Hook serves as the abstract base class for the NeuroScope intervention and monitoring system. It defines the fundamental architecture for intercepting, observing, and modifying neural network activations during forward passes, enabling sophisticated interpretability analyses and controlled interventions.

## Responsibilities

- **Lifecycle Management**: Define the standard hook execution lifecycle from registration to cleanup
- **Activation Interception**: Provide the framework for intercepting activations at specific computation points
- **Conditional Execution**: Support conditional hook activation based on model state or external criteria
- **Action Definition**: Establish the interface for defining custom hook behaviors and interventions
- **Layer Integration**: Manage attachment to specific transformer layers and components
- **State Tracking**: Maintain hook state and execution history for analysis and debugging

## Key Concepts

The Hook system implements the Observer pattern with intervention capabilities, allowing researchers to non-intrusively monitor neural network computation while also enabling controlled modifications. Each hook represents a specific analysis or intervention strategy that can be dynamically attached to and detached from model components.

Hooks operate within the forward pass execution flow, receiving activation tensors at predetermined points and either observing them (for analysis) or modifying them (for interventions). The system supports complex hook compositions and conditional execution patterns.

## Instance Variables

- **name**: A unique identifier for the hook, used for registration, debugging, and result retrieval. Should be descriptive of the hook's purpose (e.g., 'attention_head_ablation', 'residual_stream_monitor').

- **condition**: A block or predicate that determines when the hook should execute. Can depend on model state, token position, activation values, or external flags. Nil means the hook always executes.

- **action**: The core behavior block that defines what the hook does when triggered. Receives the activation tensor as input and may return a modified tensor (for interventions) or analysis results (for monitoring).

- **layer**: Specifies which transformer layer this hook is attached to. Can be a specific layer index, a layer object reference, or a symbolic identifier like #all or #embedding.

## Usage Patterns

### Basic Hook Creation
```smalltalk
"Simple monitoring hook"
monitorHook := Hook new
    name: 'layer5_monitor';
    layer: 5;
    action: [:activation | 
        Transcript show: 'Layer 5 activation mean: ', activation mean asString];
    yourself.

"Conditional intervention hook"
ablationHook := Hook new
    name: 'attention_ablation';
    layer: 8;
    condition: [:activation | ablationEnabled];
    action: [:activation | activation zeroHeads: #(3 7 11)];
    yourself.
```

### Hook Registration and Management
```smalltalk
"Register hook with model"
model hookManager addHook: monitorHook.

"Register multiple hooks"
model hookManager 
    addHook: monitorHook;
    addHook: ablationHook;
    addHook: cachingHook.

"Conditional registration"
debugMode ifTrue: [
    model hookManager addHook: debugHook].

"Remove hooks"
model hookManager removeHook: 'layer5_monitor'.
model hookManager removeAllHooks.
```

### Advanced Hook Patterns
```smalltalk
"Stateful hook with accumulation"
accumulatorHook := Hook new
    name: 'activation_accumulator';
    layer: #all;
    initialize: [activationSum := 0];
    action: [:activation | 
        activationSum := activationSum + activation sum.
        activationSum];
    finalize: [Transcript show: 'Total activation: ', activationSum asString];
    yourself.

"Multi-layer hook with coordination"
coordinatedHook := Hook new
    name: 'cross_layer_analysis';
    layer: #(5 8 11);
    action: [:activation :layerIndex | 
        layerActivations at: layerIndex put: activation.
        layerIndex = 11 ifTrue: [self analyzeAllLayers]];
    yourself.
```

## Integration Points

### With HookManager
Hooks are managed by the HookManager class, which handles registration, execution ordering, and lifecycle management. The manager ensures hooks execute at the correct points in the forward pass.

### With TransformerModel
The model's forward pass execution includes hook execution points at key computational stages. Hooks receive activations and can modify the computation flow.

### With Layer Classes
Individual layer classes (AttentionLayer, MLPLayer, etc.) provide specific hook attachment points for fine-grained intervention and monitoring.

### With ActivationTensor
Hooks operate primarily on ActivationTensor instances, leveraging their metadata and operation capabilities for sophisticated analyses.

### With Analysis Tools
Analysis classes often use hooks internally to gather data during forward passes, enabling complex interpretability workflows.

## Examples

### Complete Hook Lifecycle Example
```smalltalk
"Define a comprehensive analysis hook"
analysisHook := Hook new
    name: 'attention_pattern_analysis';
    layer: 8;
    condition: [:activation | analysisMode and: [currentToken > 5]];
    action: [:activation | 
        | patterns |
        patterns := self extractAttentionPatterns: activation.
        self recordPatterns: patterns.
        activation "Return unmodified for monitoring"
    ];
    onError: [:error | 
        Transcript show: 'Hook error: ', error messageText];
    yourself.

"Register and use"
model hookManager addHook: analysisHook.
tokens := model tokenizer encode: 'The cat sat on the mat'.
output := model forward: tokens.

"Access results"
patterns := analysisHook recordedPatterns.
statistics := analysisHook executionStatistics.
```

### Intervention Hook Example
```smalltalk
"Attention head ablation hook"
ablationHook := Hook new
    name: 'head_ablation_study';
    layer: 6;
    condition: [:activation | ablationTargets notEmpty];
    action: [:activation | 
        ablationTargets do: [:headIndex |
            activation zeroHead: headIndex].
        activation
    ];
    yourself.

"Activation patching hook"
patchingHook := Hook new
    name: 'activation_patching';
    layer: 5;
    action: [:activation | 
        patchedActivation ifNotNil: [
            activation replaceWith: patchedActivation.
        ].
        activation
    ];
    yourself.

"Use for causal analysis"
model hookManager addHook: ablationHook.

"Test different ablation targets"
#(#(3) #(7) #(3 7) #(3 7 11)) do: [:targets |
    ablationTargets := targets.
    result := model forward: testTokens.
    results at: targets put: result logits.
].
```

### Hook Composition Example
```smalltalk
"Create a pipeline of hooks"
preprocessHook := Hook new
    name: 'preprocessing';
    layer: 5;
    action: [:activation | activation normalize];
    yourself.

analysisHook := Hook new
    name: 'analysis';
    layer: 5;
    dependsOn: #('preprocessing');
    action: [:activation | 
        self analyzeNormalizedActivation: activation.
        activation
    ];
    yourself.

postprocessHook := Hook new
    name: 'postprocessing';
    layer: 5;
    dependsOn: #('analysis');
    action: [:activation | 
        self recordAnalysisResults.
        activation denormalize
    ];
    yourself.

"Register in dependency order"
model hookManager 
    addHook: preprocessHook;
    addHook: analysisHook;
    addHook: postprocessHook.
```

### Dynamic Hook Management
```smalltalk
"Runtime hook modification"
dynamicHook := Hook new
    name: 'adaptive_intervention';
    layer: 7;
    action: [:activation | 
        | interventionStrength |
        interventionStrength := self calculateInterventionStrength: activation.
        interventionStrength > threshold 
            ifTrue: [activation * 0.5]
            ifFalse: [activation]
    ];
    yourself.

"Hook with learning capability"
learningHook := Hook new
    name: 'adaptive_analysis';
    layer: 6;
    initialize: [self initializeLearningState];
    action: [:activation | 
        self updateLearningState: activation.
        self adaptAnalysisStrategy.
        activation
    ];
    yourself.

"Performance monitoring hook"
performanceHook := Hook new
    name: 'performance_monitor';
    layer: #all;
    action: [:activation :layerIndex | 
        startTime := Time millisecondClockValue.
        result := self processActivation: activation.
        endTime := Time millisecondClockValue.
        self recordExecutionTime: endTime - startTime for: layerIndex.
        result
    ];
    yourself.
```

### Error Handling and Debugging
```smalltalk
"Robust hook with error handling"
robustHook := Hook new
    name: 'robust_analysis';
    layer: 8;
    action: [:activation | 
        [self performComplexAnalysis: activation]
            on: Error
            do: [:error | 
                self logError: error.
                self useBackupAnalysis: activation
            ]
    ];
    onTimeout: 5000 do: [
        Transcript show: 'Hook timeout, skipping analysis'.
        nil
    ];
    yourself.

"Debug hook for development"
debugHook := Hook new
    name: 'debug_tracer';
    layer: #all;
    action: [:activation :layerIndex | 
        Transcript show: 'Layer ', layerIndex asString, 
                        ' shape: ', activation shape asString,
                        ' mean: ', activation mean asString.
        activation
    ];
    yourself.

"Only active in debug mode"
Smalltalk isDebugMode ifTrue: [
    model hookManager addHook: debugHook
].
```

This comprehensive documentation establishes Hook as the foundation for all intervention and monitoring capabilities in NeuroScope, providing developers with the patterns and examples needed to implement sophisticated interpretability analyses.## Me
thod Documentation

### Class Methods

#### new
**Purpose**: Creates a new Hook instance with default configuration.

**Return Value**: A new Hook instance ready for configuration

**Usage Examples**:
```smalltalk
"Basic hook creation"
hook := Hook new.

"Configured hook creation"
hook := Hook new
    name: 'my_analysis_hook';
    layer: 5;
    action: [:activation | self analyzeActivation: activation];
    yourself.
```

#### named: hookName
**Purpose**: Creates a new Hook instance with the specified name.

**Parameters**:
- `hookName` (String): Unique identifier for the hook

**Return Value**: A new Hook instance with the specified name

**Usage Examples**:
```smalltalk
"Named hook creation"
hook := Hook named: 'attention_monitor'.

"Method chaining with named hook"
hook := (Hook named: 'layer_analysis')
    layer: 8;
    action: [:activation | self processActivation: activation];
    yourself.
```

### Instance Methods

#### name: hookName
**Purpose**: Sets the unique identifier for this hook.

**Parameters**:
- `hookName` (String): Unique name for hook identification and debugging

**Return Value**: Self (for method chaining)

**Side Effects**: Updates the hook's name property

**Usage Examples**:
```smalltalk
"Set hook name"
hook name: 'residual_stream_monitor'.

"Method chaining"
hook 
    name: 'attention_ablation';
    layer: 6;
    action: [:activation | activation zeroHeads: #(2 5)].
```

#### layer: layerIdentifier
**Purpose**: Specifies which transformer layer this hook should be attached to.

**Parameters**:
- `layerIdentifier` (Integer, Symbol, or Array): Layer specification (layer index, #all, #embedding, or array of indices)

**Return Value**: Self (for method chaining)

**Usage Examples**:
```smalltalk
"Attach to specific layer"
hook layer: 5.

"Attach to all layers"
hook layer: #all.

"Attach to multiple specific layers"
hook layer: #(3 6 9).

"Attach to embedding layer"
hook layer: #embedding.
```

#### condition: conditionBlock
**Purpose**: Sets a condition that determines when the hook should execute.

**Parameters**:
- `conditionBlock` (Block): Block that receives activation and returns boolean

**Return Value**: Self (for method chaining)

**Side Effects**: Hook will only execute when condition returns true

**Usage Examples**:
```smalltalk
"Simple condition based on activation properties"
hook condition: [:activation | activation mean > 0.5].

"Condition based on external state"
hook condition: [:activation | analysisMode and: [currentStep > 10]].

"Complex multi-factor condition"
hook condition: [:activation | 
    (activation shape first > minBatchSize) and: [
        (activation max < maxActivationThreshold) and: [
            experimentalMode
        ]
    ]
].
```

#### action: actionBlock
**Purpose**: Defines the core behavior that executes when the hook is triggered.

**Parameters**:
- `actionBlock` (Block): Block that receives activation and optionally returns modified activation

**Return Value**: Self (for method chaining)

**Side Effects**: Action will be executed during forward passes when conditions are met

**Usage Examples**:
```smalltalk
"Monitoring action (no modification)"
hook action: [:activation | 
    Transcript show: 'Layer activation mean: ', activation mean asString.
    activation "Return unmodified"
].

"Intervention action (modifies activation)"
hook action: [:activation | 
    activation * 0.8 "Scale down activations"
].

"Complex analysis action"
hook action: [:activation | 
    | analysis |
    analysis := self performDetailedAnalysis: activation.
    self recordAnalysis: analysis.
    activation "Return unmodified for monitoring"
].

"Conditional intervention"
hook action: [:activation | 
    interventionActive 
        ifTrue: [activation zeroHeads: targetHeads]
        ifFalse: [activation]
].
```

#### execute: activation
**Purpose**: Executes the hook's action on the provided activation tensor.

**Parameters**:
- `activation` (ActivationTensor): The activation tensor to process

**Return Value**: The result of the action block (typically an ActivationTensor or analysis result)

**Side Effects**: 
- Executes the hook's action block
- Updates execution statistics
- May modify the activation tensor

**Usage Examples**:
```smalltalk
"Manual hook execution"
result := hook execute: activationTensor.

"Execute with error handling"
[result := hook execute: activationTensor]
    on: Error
    do: [:error | 
        Transcript show: 'Hook execution failed: ', error messageText.
        activationTensor "Return original on error"
    ].
```

**Error Conditions**:
- Raises `HookExecutionError` if action block fails
- Raises `InvalidActivationError` if activation tensor is invalid

**Performance Notes**: Execution time is tracked automatically for performance monitoring.

#### shouldExecute: activation
**Purpose**: Determines whether the hook should execute based on its condition.

**Parameters**:
- `activation` (ActivationTensor): The activation tensor to test against the condition

**Return Value**: Boolean indicating whether the hook should execute

**Usage Examples**:
```smalltalk
"Check if hook should execute"
shouldRun := hook shouldExecute: activationTensor.

"Conditional execution"
shouldRun ifTrue: [
    result := hook execute: activationTensor
].

"Batch condition checking"
activations do: [:activation |
    (hook shouldExecute: activation) ifTrue: [
        hook execute: activation
    ]
].
```

#### isActive
**Purpose**: Returns whether the hook is currently active and available for execution.

**Return Value**: Boolean indicating hook active status

**Usage Examples**:
```smalltalk
"Check hook status"
hook isActive ifTrue: [
    Transcript show: 'Hook is ready for execution'
].

"Filter active hooks"
activeHooks := allHooks select: [:hook | hook isActive].
```

#### activate
**Purpose**: Activates the hook, making it available for execution.

**Return Value**: Self (for method chaining)

**Side Effects**: Sets the hook's active status to true

**Usage Examples**:
```smalltalk
"Activate single hook"
hook activate.

"Activate multiple hooks"
hooks do: [:hook | hook activate].

"Conditional activation"
experimentalMode ifTrue: [
    experimentalHooks do: [:hook | hook activate]
].
```

#### deactivate
**Purpose**: Deactivates the hook, preventing it from executing.

**Return Value**: Self (for method chaining)

**Side Effects**: Sets the hook's active status to false

**Usage Examples**:
```smalltalk
"Deactivate hook temporarily"
hook deactivate.

"Deactivate during sensitive operations"
hook deactivate.
[self performSensitiveOperation]
    ensure: [hook activate].
```

#### reset
**Purpose**: Resets the hook's internal state and execution statistics.

**Return Value**: Self (for method chaining)

**Side Effects**: 
- Clears execution history
- Resets performance metrics
- Reinitializes any stateful components

**Usage Examples**:
```smalltalk
"Reset hook between experiments"
hook reset.

"Reset all hooks"
hooks do: [:hook | hook reset].

"Reset with confirmation"
hook hasExecutionHistory ifTrue: [
    self confirmReset ifTrue: [hook reset]
].
```

#### executionCount
**Purpose**: Returns the number of times this hook has been executed.

**Return Value**: Integer representing execution count

**Usage Examples**:
```smalltalk
"Check execution frequency"
count := hook executionCount.
Transcript show: 'Hook executed ', count asString, ' times'.

"Performance analysis"
hooks do: [:hook |
    count := hook executionCount.
    avgTime := hook averageExecutionTime.
    Transcript show: hook name, ': ', count asString, ' executions, ', 
                     avgTime asString, 'ms average'.
].
```

#### averageExecutionTime
**Purpose**: Returns the average execution time for this hook in milliseconds.

**Return Value**: Number representing average execution time

**Usage Examples**:
```smalltalk
"Performance monitoring"
avgTime := hook averageExecutionTime.
avgTime > 10 ifTrue: [
    Transcript show: 'Warning: Hook ', hook name, ' is slow (', avgTime asString, 'ms)'
].

"Performance comparison"
sortedHooks := hooks asSortedCollection: [:a :b | 
    a averageExecutionTime < b averageExecutionTime
].
```

#### lastExecutionTime
**Purpose**: Returns the execution time of the most recent hook execution.

**Return Value**: Number representing last execution time in milliseconds

**Usage Examples**:
```smalltalk
"Monitor recent performance"
lastTime := hook lastExecutionTime.
lastTime > threshold ifTrue: [
    self investigatePerformanceIssue: hook
].
```

#### totalExecutionTime
**Purpose**: Returns the cumulative execution time for all hook executions.

**Return Value**: Number representing total execution time in milliseconds

**Usage Examples**:
```smalltalk
"Calculate hook overhead"
totalTime := hook totalExecutionTime.
overhead := (totalTime / totalForwardPassTime) * 100.
Transcript show: 'Hook overhead: ', overhead asString, '%'.
```

#### hasExecutionHistory
**Purpose**: Returns whether the hook has been executed at least once.

**Return Value**: Boolean indicating if execution history exists

**Usage Examples**:
```smalltalk
"Check if hook has run"
hook hasExecutionHistory ifTrue: [
    results := hook executionResults
].

"Validate hook functionality"
hook hasExecutionHistory ifFalse: [
    Transcript show: 'Warning: Hook ', hook name, ' has never executed'
].
```

#### executionResults
**Purpose**: Returns the results from the hook's most recent execution.

**Return Value**: The result returned by the hook's action block

**Usage Examples**:
```smalltalk
"Access analysis results"
results := analysisHook executionResults.
patterns := results at: #attentionPatterns.

"Check intervention effects"
modifiedActivation := interventionHook executionResults.
difference := originalActivation - modifiedActivation.
```

#### onError: errorBlock
**Purpose**: Sets a custom error handler for hook execution failures.

**Parameters**:
- `errorBlock` (Block): Block that receives error and returns recovery action

**Return Value**: Self (for method chaining)

**Usage Examples**:
```smalltalk
"Custom error handling"
hook onError: [:error | 
    Transcript show: 'Hook error: ', error messageText.
    self logError: error.
    nil "Return nil to indicate failure"
].

"Error recovery"
hook onError: [:error | 
    error isRecoverable 
        ifTrue: [self recoverFromError: error]
        ifFalse: [error signal] "Re-raise unrecoverable errors"
].
```

#### dependsOn: hookNames
**Purpose**: Specifies other hooks that must execute before this hook.

**Parameters**:
- `hookNames` (Array of String): Names of hooks this hook depends on

**Return Value**: Self (for method chaining)

**Side Effects**: Establishes execution order dependencies

**Usage Examples**:
```smalltalk
"Single dependency"
analysisHook dependsOn: #('preprocessing_hook').

"Multiple dependencies"
finalHook dependsOn: #('data_collection' 'analysis' 'validation').

"Conditional dependencies"
advancedHook dependsOn: (
    experimentalMode 
        ifTrue: [#('experimental_prep')]
        ifFalse: [#('standard_prep')]
).
```

#### copy
**Purpose**: Creates a deep copy of the hook with independent state.

**Return Value**: A new Hook instance with copied configuration

**Usage Examples**:
```smalltalk
"Create hook variant"
baseHook := self createBaseAnalysisHook.
layer5Hook := baseHook copy layer: 5.
layer8Hook := baseHook copy layer: 8.

"Template-based hook creation"
template := Hook new
    condition: [:activation | activation mean > threshold];
    action: [:activation | self standardAnalysis: activation];
    yourself.

specificHooks := (1 to: 12) collect: [:layerIndex |
    template copy 
        name: 'analysis_layer_', layerIndex asString;
        layer: layerIndex;
        yourself
].
```

#### printOn: stream
**Purpose**: Provides a string representation of the hook for debugging and logging.

**Parameters**:
- `stream` (WriteStream): Stream to write the description to

**Return Value**: The stream (for method chaining)

**Usage Examples**:
```smalltalk
"Debug output"
Transcript show: hook printString.

"Logging"
logger log: 'Registering hook: ', hook printString.

"Hook inventory"
hooks do: [:hook |
    Transcript show: hook printString; cr
].
```