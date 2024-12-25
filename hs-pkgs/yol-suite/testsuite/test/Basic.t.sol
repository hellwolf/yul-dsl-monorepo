pragma solidity ^0.8.20;

import { Test, console2 } from "forge-std/Test.sol";

import { IBasicTestsProgram, createBasicTestsProgram } from "yol-build/Contracts.sol";


contract BasicProgramTest is Test {
  IBasicTestsProgram private _p;

  constructor () {
    _p = createBasicTestsProgram();
  }

  function testEmbeddings() external {
    _p.embUnit$p(42);
    assertTrue(_p.embTrue$p(), "embTrue$p");
    assertTrue(_p.embTrue$l(), "embTrue$l");
  }

  function testRangeSum() external {
    assertEq(_p.rangeSum$p(0, 1, 10), 55, "result1");
  }
}
