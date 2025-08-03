//! # p-ATA: pinocchio Associated Token Account Program
//!
//! An optimized implementation of the Associated Token Account (ATA) program

#![no_std]

pub mod account;
pub mod entrypoint;
pub mod processor;
pub mod recover;
pub mod size;

#[cfg(any(test, feature = "std"))]
pub mod test_helpers;
#[cfg(any(test, feature = "std"))]
pub mod test_utils;
#[cfg(any(test, feature = "std"))]
extern crate std;
