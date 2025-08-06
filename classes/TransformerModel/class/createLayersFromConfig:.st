createLayersFromConfig: config
	"Creates an ordered collection of TransformerLayer instances based on the model configuration.
	
	Parameters:
	- config (Dictionary): The model configuration dictionary
	
	Returns: OrderedCollection of TransformerLayer instances"
	
	| numLayers hiddenSize numHeads layers |
	
	"Extract layer configuration parameters"
	numLayers := config at: #num_hidden_layers ifAbsent: [
		config at: #n_layer ifAbsent: [12]  "Default for GPT-2 small"
	].
	
	hiddenSize := config at: #hidden_size ifAbsent: [
		config at: #n_embd ifAbsent: [768]  "Default for GPT-2 small"
	].
	
	numHeads := config at: #num_attention_heads ifAbsent: [
		config at: #n_head ifAbsent: [12]  "Default for GPT-2 small"
	].
	
	"Validate configuration parameters"
	(hiddenSize \\ numHeads = 0) ifFalse: [
		ConfigurationError signal: 'Hidden size must be divisible by number of attention heads'
	].
	
	"Create transformer layers"
	layers := OrderedCollection new: numLayers.
	
	0 to: numLayers - 1 do: [:layerIndex |
		| layer |
		layer := TransformerLayer new
			layerIndex: layerIndex;
			hiddenSize: hiddenSize;
			numAttentionHeads: numHeads;
			config: config;
			yourself.
		layers add: layer
	].
	
	^layers