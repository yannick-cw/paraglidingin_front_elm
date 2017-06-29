
## Paragliding Frontend App
An elm frontend served by a haskell server		

### Stack Setup
Setup Stack witch `stack setup`

### How to run the development environment		
`stack build && stack exec haskellServer-exe` Runs he application and serves the on `localhost:8081`		
		
Start a watcher for the elm project so that the elm compiler starts after file change.  		
`cd elm/ && npm i && npm run watch`