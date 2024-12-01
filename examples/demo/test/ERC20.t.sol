pragma solidity ^0.8.20;

import { Test, console } from "forge-std/Test.sol";

import { BasicProgram, ERC20Program } from "yol-build/Contracts.sol";


interface ERC20 {
  function transfer(address, address, uint) external;
}

contract ERC20ProgramTest is Test {
  ERC20Program private token;

  constructor () {
    token = new ERC20Program();
  }

  function testTransfer() external {
    ERC20Program token1 = new ERC20Program();
    assertNotEq(address(token1), address(0));
    // TODO: need stunt contract
    ERC20(address(token1)).transfer(address(1), address(2), 3);
  }
}
