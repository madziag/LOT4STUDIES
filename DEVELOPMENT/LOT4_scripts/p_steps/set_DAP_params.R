# sets DAP parameters
is_ARS<-F
is_BIFAP<-F
is_CASERTA<-F
is_CPRD<-F
is_Denmark<-F
is_FISABIO<-F
is_PHARMO<-F

if(DAP_name=="ARS"){is_ARS<-T}    
if(DAP_name=="BIFAP"){is_BIFAP<-T}#multiple regions  
if(DAP_name=="CASERTA"){is_CASERTA<-T} 
if(DAP_name=="CPRD"){is_CPRD<-T}    
if(DAP_name=="DNR"){is_Denmark<-T}#customized denominator 
if(DAP_name=="FISABIO"){is_FISABIO<-T}
if(DAP_name=="PHARMO"){is_PHARMO<-T}#records up to 2019 