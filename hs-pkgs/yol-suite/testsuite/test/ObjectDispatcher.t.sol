pragma solidity ^0.8.20;

import { Test } from "forge-std/Test.sol";

import { ObjectDispatcherTestsProgram } from "yol-build/Contracts.sol";

contract ObjectDispatcherTest is Test {
  ObjectDispatcherTestsProgram private prog;

  constructor () {
    prog = new ObjectDispatcherTestsProgram();
    assertNotEq(address(prog), address(0));
  }

  function test_init() external {
    // TODO: need stunt contract
    // ERC20(address(token1)).transfer(address(1), address(2), 3);
  }
}
