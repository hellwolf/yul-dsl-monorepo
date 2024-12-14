pragma solidity ^0.8.20;

import { Test, console } from "forge-std/Test.sol";

import { BasicProgram } from "yol-build/Contracts.sol";


interface IBasicProgram {
  function foo1(uint256 x) external pure returns (uint256);
  function foo2(uint256 x1, uint256 x2) external pure returns (uint256);
  function rangeSumL(uint256 from, uint256 step, uint256 until) external pure returns (uint256 sum);
  function rangeSumV1(uint256 from, uint256 step, uint256 until) external pure returns (uint256 sum);
  function rangeSumV2(uint256 from, uint256 step, uint256 until) external pure returns (uint256 sum);
}

contract BasicProgramTest is Test {
  IBasicProgram private _p;

  constructor () {
    _p = IBasicProgram(address(new BasicProgram()));
  }

  /* function testFoo1(uint128 x) external { */
  /*   uint256 result = _p.foo1(x); */
  /*   console.log("result", result); */
  /*   assertEq(result, 2 * uint256(x)); */
  /* } */

  /* function testFoo2(uint128 x1, uint128 x2) external { */
  /*   uint256 result = _p.foo2(x1, x2); */
  /*   assertEq(result, uint256(x1) + uint256(x2) * 2); */
  /* } */

  function testRangeSumFnL() external {
    uint256 result = _p.rangeSumL(0, 1, 10);
    console.log("result", result);
    assertEq(result, 55);
  }

  function testRangeSumFnV() external {
    uint256 result1 = _p.rangeSumV1(0, 1, 10);
    uint256 result2 = _p.rangeSumV1(0, 1, 10);
    console.log("result1", result1);
    console.log("result2", result2);
    assertEq(result1, 55);
    assertEq(result2, 55);
  }
}
