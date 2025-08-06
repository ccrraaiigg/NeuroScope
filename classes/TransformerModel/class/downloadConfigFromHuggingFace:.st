downloadConfigFromHuggingFace: modelName
	"Downloads and parses the config.json file from HuggingFace Hub for the specified model.
	
	Parameters:
	- modelName (String): The HuggingFace model identifier
	
	Returns: Dictionary containing the parsed configuration, or nil if download fails"
	
	| url configJson |
	
	"Construct HuggingFace Hub URL for config file"
	url := 'https://huggingface.co/', modelName, '/resolve/main/config.json'.
	
	"Download configuration file"
	[
		configJson := JSInterface httpGet: url.
		configJson ifNil: [^nil].
		
		"Parse JSON configuration"
		^JSON parse: configJson
	] on: Error do: [:ex |
		"Handle network errors, JSON parsing errors, etc."
		NetworkError signal: 'Failed to download model configuration: ', ex messageText.
		^nil
	]