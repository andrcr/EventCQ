{ 
module Grammar where 
import Tokens 
}

%name gram 
%tokentype { Token } 
%error { parseError }
%token
    '-' {TokenDif }
	'=' {TokenEq }
    var { TokenVar $$ } 
    '(' { TokenLParen } 
    ')' { TokenRParen } 
 
%% 
FileName : var '(' ReadOrder ')' FileName                                    { Filename $1 $3 $5 }
         | var '(' ReadOrder ')' PrintOrder                                  { Filenam $1 $3 $5 }
         | var '(' ReadOrder ')' '-' ExOrder '-' PrintOrder                  { Filename1 $1 $3 $6 $8 }
         | var '(' ReadOrder ')' '=' EgOrder '=' PrintOrder                  { Filename2 $1 $3 $6 $8 }
         | var '(' ReadOrder ')' '-' ExOrder '-' '=' EgOrder '=' PrintOrder  { Filename3 $1 $3 $6 $9 $11 }
         | var '(' ReadOrder ')' '=' EgOrder '=' '-' ExOrder '-' PrintOrder  { Filename4 $1 $3 $6 $9 $11 }

ReadOrder : var ReadOrder                                                    { OrdName $1 $2 }
          | var                                                              { OrdNam $1 }

PrintOrder : var PrintOrder                                                  { PrintName $1 $2 }
           | var                                                             { PrintNam $1 }

ExOrder : var ExOrder                                                        { ExName $1 $2 }
        | var                                                                { ExNam $1 }

EgOrder : var '=' var EgOrder                                                    { EgName $1 $3 $4 }
        | var '=' var                                                            { EgNam $1 $3 }
	
    
{ 
parseError :: [Token] -> a
parseError _ = error "Parse error" 

data FileName = Filename String ReadOrder FileName
              | Filenam String ReadOrder PrintOrder
              | Filename1 String ReadOrder ExOrder PrintOrder
              | Filename2 String ReadOrder EgOrder PrintOrder
              | Filename3 String ReadOrder ExOrder EgOrder PrintOrder
              | Filename4 String ReadOrder EgOrder ExOrder PrintOrder
              deriving (Show)

data ExOrder = ExName String ExOrder
             | ExNam String
             deriving (Show)

data EgOrder = EgName String String EgOrder
             | EgNam String String
             deriving (Show)

data ReadOrder = OrdName String ReadOrder 
               | OrdNam String
			   deriving (Show)

data PrintOrder = PrintName String PrintOrder
                | PrintNam String 
                deriving (Show)
} 