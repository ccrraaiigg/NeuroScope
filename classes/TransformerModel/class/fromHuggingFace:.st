fromHuggingFace: modelName
	"Creates a new TransformerModel instance by loading a pre-trained model from the HuggingFace Hub.
	
	Parameters:
	- modelName (String): The HuggingFace model identifier (e.g., 'gpt2-small', 'bert-base-uncased')
	
	Returns: A fully initialized TransformerModel instance with loaded weights and configuration"
	
	| config tokenizer layers model |
	
	"Validate input parameters"
	modelName ifNil: [self error: 'Model name cannot be nil'].
	modelName isEmpty ifTrue: [self error: 'Model name cannot be empty'].
	
	"Download and parse model configuration"
	config := self downloadConfigFromHuggingFace: modelName.
	config ifNil: [
		ModelNotFoundError signal: 'Model "', modelName, '" not found on HuggingFace Hub'
	].
	
	"Validate model architecture compatibility"
	(self isSupportedArchitecture: config) ifFalse: [
		ConfigurationError signal: 'Unsupported model architecture: ', (config at: #architectures ifAbsent: ['unknown'])
	].
	
	"Initialize tokenizer"
	tokenizer := Tokenizer fromHuggingFace: modelName.
	
	"Create and initialize transformer layers"
	layers := self createLayersFromConfig: config.
	
	"Load pre-trained weights"
	self loadWeightsFromHuggingFace: modelName intoLayers: layers.
	
	"Create and configure model instance"
	model := self new.
	model 
		initializeWithLayers: layers
		tokenizer: tokenizer
		config: config.
	
	"Validate loaded model"
	model validateConfiguration.
	
	^model