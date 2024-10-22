pragma solidity ^0.8.20;

import { Test, console } from "forge-std/Test.sol";

import { BasicProgram, ERC20Program } from "yol-build/Contracts.sol";


interface ERC20 {
  function transfer(address, address, uint) external;
}

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
    uint256 result = _p.rangeSumLFn(1, 1, 10);
    console.log("result", result);
    assertEq(result, 55);
  }

  /* function testRangeSumVFn() external { */
  /*   console.log("result", _p.rangeSumLFn(0, 1, 2)); */
  /*   assertEq(_p.rangeSumVFn(0, 1, 2), 0, "not equal"); */
  /* } */
}

contract ERC20ProgramTest is Test {
  ERC20Program private token;

  constructor () {
    token = new ERC20Program();
  }

  function testInit() external {
    ERC20Program token1 = new ERC20Program();
    assertNotEq(address(token1), address(0));
    // TODO: need stunt contract
    ERC20(address(token1)).transfer(address(1), address(2), 3);
  }
}
