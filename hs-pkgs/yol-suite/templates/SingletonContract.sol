contract %pname% is Proxy {
    address immutable public LOGIC_ADDRESS;

    constructor () {
        bytes memory bytecode = "%bytecode%";
        address logicAddress;

        // create both logic and stunt
        // TODO stunt = new ERC20Stunt();
        assembly {
            // sstore(IMPLEMENTATION_SLOT, sload(stunt.slot))
            logicAddress := create(0, add(bytecode, 0x20), mload(bytecode))
        }
        assert(logicAddress != address(0));
        LOGIC_ADDRESS = logicAddress;

    }

    function _implementation() internal view override returns (address) {
        return LOGIC_ADDRESS;
    }
}

function create%pname%() returns (%iname%){
  return %iname%(address(new %pname%()));
}
