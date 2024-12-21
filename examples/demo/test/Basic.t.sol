pragma solidity ^0.8.20;

import { Test, console2 } from "forge-std/Test.sol";

import { IBasicProgram, createBasicProgram } from "yol-build/Contracts.sol";


contract BasicProgramTest is Test {
  IBasicProgram private _p;

  constructor () {
    _p = createBasicProgram();
  }

  function testFoo1(int128 x) external {
    int256 result = _p.foo1(x);
    console2.log("result", result);
    if (x > 0) {
      assertEq(result, 2 * int256(x));
    } else {
      assertEq(result, 0);
    }
  }

  function testFoo2(uint32 x1, uint32 x2) external {
    uint256 result = _p.foo2(x1, x2);
    if (uint256(x1) + uint256(x2) > type(uint32).max) {
      assertEq(result, 0, "should overflow");
    } else {
      assertEq(result, uint256(x1) + uint256(x2), "just value");
    }
  }

  function testRangeSumFnL() external {
    uint256 result = _p.rangeSumL(0, 1, 10);
    console2.log("result", result);
    assertEq(result, 55);
  }

  function testRangeSumFnV() external {
    uint256 result1 = _p.rangeSumV1(0, 1, 10);
    uint256 result2 = _p.rangeSumV1(0, 1, 10);
    console2.log("result1", result1);
    console2.log("result2", result2);
    assertEq(result1, 55);
    assertEq(result2, 55);
  }
}
