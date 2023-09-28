pragma solidity ^0.8.20;

import { Test } from "forge-std/Test.sol";

import { ERC20Program } from "yol-build/Contracts.sol";

contract ERC20ProgramTest is Test {
  ERC20Program private token;

  constructor () {
    token = new ERC20Program();
  }

  function test_init() external {
    ERC20Program token1 = new ERC20Program();
    assertNotEq(address(token1), address(0));
  }
}
