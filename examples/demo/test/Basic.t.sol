pragma solidity ^0.8.20;

import { Test, console } from "forge-std/Test.sol";

import { BasicProgram } from "yol-build/Contracts.sol";


interface IBasicProgram {
  function foo1(uint256 x) external pure returns (uint256);
  function foo2(uint256 x1, uint256 x2) external pure returns (uint256);
  function rangeSumLFn(uint256 from, uint256 step, uint256 until) external pure returns (uint256 sum);
  function rangeSumVFn(uint256 from, uint256 step, uint256 until) external pure returns (uint256 sum);
}

contract BasicProgramTest is Test {
  IBasicProgram private _p;

  constructor () {
    _p = IBasicProgram(address(new BasicProgram()));
  }

  function testFoo1(uint128 x) external {
    uint256 result = _p.foo1(x);
    console.log("result", result);
    assertEq(result, 2 * uint256(x));
  }

  function testFoo2(uint128 x1, uint128 x2) external {
    uint256 result = _p.foo2(x1, x2);
    assertEq(result, uint256(x1) + uint256(x2) * 2);
  }

  function testRangeSumLFn() external {
    uint256 result = _p.rangeSumLFn(0, 1, 10);
    console.log("result", result);
    assertEq(result, 55);
  }

  function testRangeSumVFn() external {
    uint256 result = _p.rangeSumVFn(0, 1, 10);
    console.log("result", result);
    assertEq(result, 55);
  }
}
