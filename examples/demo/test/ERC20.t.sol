pragma solidity ^0.8.20;

import { Test, console } from "forge-std/Test.sol";

import { IERC20Program, createERC20Program } from "yol-build/Contracts.sol";


contract ERC20ProgramTest is Test {
  IERC20Program private token;

  constructor () {
    token = createERC20Program();
  }

  function testTransfer() external {
    token.transfer(address(1), address(2), 3);
  }
}
