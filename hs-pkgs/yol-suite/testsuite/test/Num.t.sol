pragma solidity ^0.8.20;

import { Test, console2 } from "forge-std/Test.sol";

import { NumTestsProgram } from "yol-build/Contracts.sol";


interface INumTestsProgram {
  function add_uint256(uint256, uint256) external pure returns (uint256);
  function add_uint128(uint128, uint128) external pure returns (uint128);
  function add_int256(int256, int256) external pure returns (int256);
  function add_int128(int128, int128) external pure returns (int128);
}

function panicError(uint256 errCode) pure returns (bytes memory) {
  return abi.encodeWithSelector(bytes4(0x4e487b71), errCode);
}

contract T is Test {
  INumTestsProgram private prog;

  constructor () {
    prog = INumTestsProgram(address(new NumTestsProgram()));
  }

  function test_add_uint256(uint256 x, uint256 y) external {
    if (y <= type(uint256).max - x) {
      assertEq(prog.add_uint256(x, y), x + y);
    } else {
      vm.expectRevert(panicError(0x11));
      uint256 z = prog.add_uint256(x, y);
      console2.log(z);
    }
  }

  function test_add_uint128(uint128 x, uint128 y) external {
    if (y <= type(uint128).max - x) {
      assertEq(prog.add_uint128(x, y), x + y);
    } else {
      vm.expectRevert(panicError(0x11));
      uint128 z = prog.add_uint128(x, y);
      console2.log(z);
    }
  }

  function test_add_int256(int256 x, int256 y) external {
    if ((x > 0 && y <= type(int256).max - x) ||
        (x < 0 && y >= type(int256).min - x) ||
        x == 0) {
      assertEq(prog.add_int256(x, y), x + y);
    } else {
      vm.expectRevert(panicError(0x11));
      int256 z = prog.add_int256(x, y);
      console2.log(z);
    }
  }

  function test_add_int128(int128 x, int128 y) external {
    if ((x > 0 && y <= type(int128).max - x) ||
        (x < 0 && y >= type(int128).min - x) ||
        x == 0) {
      assertEq(prog.add_int128(x, y), x + y);
    } else {
      vm.expectRevert(panicError(0x11));
      int128 z = prog.add_int128(x, y);
      console2.log(z);
    }
  }
}
