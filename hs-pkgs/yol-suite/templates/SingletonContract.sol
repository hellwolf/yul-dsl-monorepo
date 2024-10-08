contract %pname% is Proxy {
    address public logic;

    constructor () {
        bytes memory bytecode = "%bytecode%";

        // create both logic and stunt
        // stunt = new ERC20Stunt();
        assembly {
            // sstore(IMPLEMENTATION_SLOT, sload(stunt.slot))
            sstore(logic.slot, create(0, add(bytecode, 0x20), mload(bytecode)))
        }

    }

    function _implementation() internal view override returns (address) {
        return logic;
    }
}
