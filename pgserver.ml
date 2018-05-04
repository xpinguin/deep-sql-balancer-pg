module PGProtocol = struct
    type Header = {
        op : char; (*/1*)
        len : int; (*/4*)
    }
    
    type AuthenticatonOk = {
        h : Header;
        subop : char; (*==0 /1*)
    }
    type AuthenticationMD5Password = {
        h : Header;
        subop : char; (*==5 /1*)
        salt : array; (*/[4]*)
    }
    
    type BackendKeyData = {
        h : Header;
        backend_pid : int; (*/4*)
        key : int; (*/4*)
    }
    type CancelRequest = {
        h : Header;
        backend_pid : int; (*/4*)
        key : int; (*/4*)
    }
    
    type CommandComplete = {
        h : Header;
        tag : string;
    }
    (*type ErrorResponse = {
        
    }*)
    
end
