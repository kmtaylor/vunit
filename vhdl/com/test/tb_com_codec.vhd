-- Test suite for com codec package
--
-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this file,
-- You can obtain one at http://mozilla.org/MPL/2.0/.
--
-- Copyright (c) 2015, Lars Asplund lars.anders.asplund@gmail.com

library vunit_lib;
context vunit_lib.vunit_context;
context vunit_lib.com_context;

library tb_com_lib;
use tb_com_lib.custom_codec_pkg.all;
use tb_com_lib.custom_types_pkg.all;

library ieee;
use ieee.std_logic_1164.all;
use ieee.float_pkg.all;
use ieee.math_complex.all;
use ieee.numeric_bit.all;
use ieee.numeric_std.all;
use ieee.fixed_pkg.all;

use std.textio.all;

entity tb_com_codec is
  generic (
    runner_cfg : runner_cfg_t := runner_cfg_default);
end entity tb_com_codec;

architecture test_fixture of tb_com_codec is
begin
  test_runner : process
    -- Standard "=" for these types return false when both operands are empty
    -- vectors. However, I want decode(encode("")) = "" to return true when verifying that
    -- empty vectors can be encoded/decoded correctly
    function "=" (
      constant l, r : ieee.numeric_bit.unsigned)
      return boolean is
      variable ret_val : boolean;
    begin
      if l'length = 0 and r'length = 0 then
        return true;
      end if;

      return ieee.numeric_bit."="(l, r);
    end function "=";

    function "=" (
      constant l, r : ieee.numeric_bit.signed)
      return boolean is
      variable ret_val : boolean;
    begin
      if l'length = 0 and r'length = 0 then
        return true;
      end if;

      return ieee.numeric_bit."="(l, r);
    end function "=";

    function "=" (
      constant l, r : ieee.numeric_std.unsigned)
      return boolean is
      variable ret_val : boolean;
    begin
      if l'length = 0 and r'length = 0 then
        return true;
      end if;

      return ieee.numeric_std."="(l, r);
    end function "=";

    function "=" (
      constant l, r : ieee.numeric_std.signed)
      return boolean is
      variable ret_val : boolean;
    begin
      if l'length = 0 and r'length = 0 then
        return true;
      end if;

      return ieee.numeric_std."="(l, r);
    end function "=";

    variable f64    : float64;
    variable r1, r2 : real;
    constant positive_zero : float64 := to_float(
      std_logic_vector'(B"0_00000000000_0000000000000000000000000000000000000000000000000000"), f64);
    constant negative_zero : float64 := to_float(
      std_logic_vector'(B"1_00000000000_0000000000000000000000000000000000000000000000000000"), f64);
    constant positive_infinity : float64 := to_float(
      std_logic_vector'(B"0_11111111111_0000000000000000000000000000000000000000000000000000"), f64);
    constant negative_infinity : float64 := to_float(
      std_logic_vector'(B"1_11111111111_0000000000000000000000000000000000000000000000000000"), f64);
    constant nan : float64 := to_float(
      std_logic_vector'(B"1_11111111111_0000000000000000000000000000000000000000000000000001"), f64);
    constant special_chars       : string(1 to 3) := "),(";
    constant comma               : character      := ',';
    constant lp                  : character      := '(';
    constant rp                  : character      := ')';
    variable null_string         : string(10 to 9);
    variable null_boolean_vector : boolean_vector(1 to 0);
    variable null_integer_vector : integer_vector(1 to 0);
    variable null_real_vector    : real_vector(1 to 0);
    variable null_time_vector    : time_vector(1 to 0);
    variable null_array4_t       : array4_t(10 to 8);
    variable null_array5_t       : array5_t(1 to 0, 1 to 0);
    variable null_array5_2_t     : array5_t(0 to 1, 1 to 0);
    variable null_array5_3_t     : array5_t(1 to 0, 0 to 1);
    variable null_array6_t       : array6_t(apple downto banana);
    variable null_array7_t       : array7_t(1 to 2, apple downto banana);
    variable t1, t2, t3, t4, t5  : time;
    variable my_record4          : record4_t;
    variable my_record5          : record5_t;
    variable my_record6          : record6_t;
    variable my_record7          : record7_t;
    variable e1, e2, e3          : line;
  begin
    checker_init(display_format => verbose,
                 file_name      => join(output_path(runner_cfg), "error.csv"),
                 file_format    => verbose_csv);
    test_runner_setup(runner, runner_cfg);

    while test_suite loop
      if run("Test that integer can be encoded and decoded") then
        check_relation(decode_integer(encode_integer(integer'low)) = integer'low);
        check_relation(decode_integer(encode_integer(integer'high)) = integer'high);
      elsif run("Test that real can be encoded and decoded") then
        check_relation(decode_real(encode_real(real'low)) = real'low);
        check_relation(decode_real(encode_real(real'high)) = real'high);

        check_relation(decode_real(encode_real(to_real(positive_zero))) = to_real(positive_zero));
        check_relation(decode_real(encode_real(to_real(negative_zero))) = to_real(negative_zero));
        check_relation(decode_real(encode_real(to_real(positive_infinity))) = to_real(positive_infinity));
        check_relation(decode_real(encode_real(to_real(negative_infinity))) = to_real(negative_infinity));
        check_relation(decode_real(encode_real(to_real(nan))) = to_real(nan));
        -- ModelSim doesn't support float meta values for the real type. Positive/negative zero as
        -- well as NaN get the same internal representation (positive
        -- zero). Positive/negative infinity seem to maintain correct internal
        -- representation but doing things like 1.0/0.0 isn't supported. The tests
        -- for encoding/decoding of these values still pass so I've kept them for tests
        -- with other simulators
--        check_relation(decode_real(encode_real(to_real(nan))) /= to_real(positive_zero));
--        check_relation(decode_real(encode_real(to_real(negative_zero))) /= to_real(positive_zero));

        r1 := to_real(to_float(
          std_logic_vector'(B"0_01111111111_0000000000000000000000000000000000000000000000000001"), f64));
        r2 := to_real(to_float(
          std_logic_vector'(B"0_01111111111_0000000000000000000000000000000000000000000000000010"), f64));
        check_relation(decode_real(encode_real(r1)) = r1);
        check_relation(decode_real(encode_real(r2)) = r2);
        check_relation(r1 /= r2, "Should be different values in a double precision implementation");
      elsif run("Test that time can be encoded and decoded") then
        check_relation(decode_time(encode_time(time'low)) = time'low);
        check_relation(decode_time(encode_time(time'high)) = time'high);
        check_relation(decode_time(encode_time(17 ns)) = 17 ns);
        check_relation(decode_time(encode_time(-17 ns)) = -17 ns);
      elsif run("Test that boolean can be encoded and decoded") then
        check_relation(decode_boolean(encode_boolean(true)) = true);
        check_relation(decode_boolean(encode_boolean(false)) = false);
      elsif run("Test that bit can be encoded and decoded") then
        check_relation(decode_bit(encode_bit('0')) = bit'('0'));
        check_relation(decode_bit(encode_bit('1')) = bit'('1'));
      elsif run("Test that std_ulogic can be encoded and decoded") then
        for i in std_ulogic'pos(std_ulogic'left) to std_ulogic'pos(std_ulogic'right) loop
          check_relation(decode_std_ulogic(encode_std_ulogic(std_ulogic'val(i))) = std_ulogic'val(i));
        end loop;
      elsif run("Test that severity_level can be encoded and decoded") then
        for i in severity_level'pos(severity_level'left) to severity_level'pos(severity_level'right) loop
          check_relation(decode_severity_level(encode_severity_level(severity_level'val(i))) = severity_level'val(i));
        end loop;
      elsif run("Test that file_open_status can be encoded and decoded") then
        for i in file_open_status'pos(file_open_status'left) to file_open_status'pos(file_open_status'right) loop
          check_relation(decode_file_open_status(encode_file_open_status(file_open_status'val(i))) = file_open_status'val(i));
        end loop;
      elsif run("Test that file_open_kind can be encoded and decoded") then
        for i in file_open_kind'pos(file_open_kind'left) to file_open_kind'pos(file_open_kind'right) loop
          check_relation(decode_file_open_kind(encode_file_open_kind(file_open_kind'val(i))) = file_open_kind'val(i));
        end loop;
      elsif run("Test that character can be encoded and decoded") then
        for i in character'pos(character'left) to character'pos(character'right) loop
          check_relation(decode_character(encode_character(character'val(i))) = character'val(i));
        end loop;
      elsif run("Test that string can be encoded and decoded") then
        check_relation(decode_string(encode_string("The quick brown fox jumps over the lazy dog")) = string'("The quick brown fox jumps over the lazy dog"));
        check_relation(decode_string(encode_string(special_chars)) = string'(special_chars));
        check_relation(decode_string(encode_string("")) = string'(""));
        check_relation(decode_string(encode_string(null_string))'left = 10);
        check_relation(decode_string(encode_string(null_string))'right = 9);
        check_relation(decode_string(encode_string((15 downto 4 => "Hello world!"))) = string'("Hello world!"));
        check_relation(decode_string(encode_string((15 downto 4 => "Hello world!")))'left = 15);
        check_relation(decode_string(encode_string((15 downto 4 => "Hello world!")))'right = 4);
      elsif run("Test that boolean_vector can be encoded and decoded") then
        check_relation(decode_boolean_vector(encode_boolean_vector((true, false, true))) = boolean_vector'((true, false, true)));
        check_relation(decode_boolean_vector(encode_boolean_vector((0          => true))) = boolean_vector'((0 => true)));
        check_relation(decode_boolean_vector(encode_boolean_vector(null_boolean_vector)) = null_boolean_vector);
        check_relation(decode_boolean_vector(encode_boolean_vector((5 downto 3 => (true, false, true)))) = boolean_vector'((true, false, true)));
        check_relation(decode_boolean_vector(encode_boolean_vector((5 downto 3 => (true, false, true))))'left = 5);
        check_relation(decode_boolean_vector(encode_boolean_vector((5 downto 3 => (true, false, true))))'right = 3);
      elsif run("Test that bit_vector can be encoded and decoded") then
        check_relation(decode_bit_vector(encode_bit_vector("101")) = bit_vector'("101"));
        check_relation(decode_bit_vector(encode_bit_vector("1")) = bit_vector'("1"));
        check_relation(decode_bit_vector(encode_bit_vector("")) = bit_vector'(""));
        check_relation(decode_bit_vector(encode_bit_vector((5 downto 3 => "101"))) = bit_vector'("101"));
        check_relation(decode_bit_vector(encode_bit_vector((5 downto 3 => "101")))'left = 5);
        check_relation(decode_bit_vector(encode_bit_vector((5 downto 3 => "101")))'right = 3);
      elsif run("Test that integer_vector can be encoded and decoded") then
        check_relation(decode_integer_vector(encode_integer_vector((-42, 0, 17))) = integer_vector'((-42, 0, 17)));
        check_relation(decode_integer_vector(encode_integer_vector((0          => -42))) = integer_vector'((0 => -42)));
        check_relation(decode_integer_vector(encode_integer_vector(null_integer_vector)) = null_integer_vector);
        check_relation(decode_integer_vector(encode_integer_vector((5 downto 3 => (-42, 0, 17)))) = integer_vector'((-42, 0, 17)));
        check_relation(decode_integer_vector(encode_integer_vector((5 downto 3 => (-42, 0, 17))))'left = 5);
        check_relation(decode_integer_vector(encode_integer_vector((5 downto 3 => (-42, 0, 17))))'right = 3);
      elsif run("Test that real_vector can be encoded and decoded") then
        check_relation(decode_real_vector(encode_real_vector((-42.42, 0.001, 17.17))) = real_vector'((-42.42, 0.001, 17.17)));
        check_relation(decode_real_vector(encode_real_vector((0          => -42.42))) = real_vector'((0 => -42.42)));
        check_relation(decode_real_vector(encode_real_vector(null_real_vector)) = null_real_vector);
        check_relation(decode_real_vector(encode_real_vector((5 downto 3 => (-42.42, 0.001, 17.17)))) = real_vector'((-42.42, 0.001, 17.17)));
        check_relation(decode_real_vector(encode_real_vector((5 downto 3 => (-42.42, 0.001, 17.17))))'left = 5);
        check_relation(decode_real_vector(encode_real_vector((5 downto 3 => (-42.42, 0.001, 17.17))))'right = 3);
      elsif run("Test that time_vector can be encoded and decoded") then
        check_relation(decode_time_vector(encode_time_vector((-42 ms, 0 sec, 17 min))) = time_vector'((-42 ms, 0 sec, 17 min)));
        check_relation(decode_time_vector(encode_time_vector((0          => -42 ms))) = time_vector'((0 => -42 ms)));
        check_relation(decode_time_vector(encode_time_vector(null_time_vector)) = null_time_vector);
        check_relation(decode_time_vector(encode_time_vector((5 downto 3 => (-42 ms, 0 sec, 17 min)))) = time_vector'((-42 ms, 0 sec, 17 min)));
        check_relation(decode_time_vector(encode_time_vector((5 downto 3 => (-42 ms, 0 sec, 17 min))))'left = 5);
        check_relation(decode_time_vector(encode_time_vector((5 downto 3 => (-42 ms, 0 sec, 17 min))))'right = 3);
      elsif run("Test that std_ulogic_vector can be encoded and decoded") then
        check_relation(decode_std_ulogic_vector(encode_std_ulogic_vector("XU1")) = std_ulogic_vector'("XU1"));
        check_relation(decode_std_ulogic_vector(encode_std_ulogic_vector("X")) = std_ulogic_vector'("X"));
        check_relation(decode_std_ulogic_vector(encode_std_ulogic_vector("")) = std_ulogic_vector'(""));
        check_relation(decode_std_ulogic_vector(encode_std_ulogic_vector((5 downto 3 => "XU1"))) = std_ulogic_vector'("XU1"));
        check_relation(decode_std_ulogic_vector(encode_std_ulogic_vector((5 downto 3 => "XU1")))'left = 5);
        check_relation(decode_std_ulogic_vector(encode_std_ulogic_vector((5 downto 3 => "XU1")))'right = 3);
      elsif run("Test that complex can be encoded and decoded") then
        check_relation(decode_complex(encode_complex((-17.17, 42.42))) = complex'((-17.17, 42.42)));
      elsif run("Test that complex_polar can be encoded and decoded") then
        check_relation(decode_complex_polar(encode_complex_polar((17.17, 0.42))) = complex_polar'((17.17, 0.42)));
      elsif run("Test that unsigned from numeric_bit can be encoded and decoded") then
        check_relation(decode_numeric_bit_unsigned(encode_numeric_bit_unsigned("101")) = ieee.numeric_bit.unsigned'("101"));
        check_relation(decode_numeric_bit_unsigned(encode_numeric_bit_unsigned("1")) = ieee.numeric_bit.unsigned'("1"));
        check_relation(decode_numeric_bit_unsigned(encode_numeric_bit_unsigned("")) = ieee.numeric_bit.unsigned'(""));
        check_relation(decode_numeric_bit_unsigned(encode_numeric_bit_unsigned((5 downto 3 => "101"))) = ieee.numeric_bit.unsigned'("101"));
        check_relation(decode_numeric_bit_unsigned(encode_numeric_bit_unsigned((5 downto 3 => "101")))'left = 5);
        check_relation(decode_numeric_bit_unsigned(encode_numeric_bit_unsigned((5 downto 3 => "101")))'right = 3);
      elsif run("Test that signed from numeric_bit can be encoded and decoded") then
        check_relation(decode_numeric_bit_signed(encode_numeric_bit_signed("101")) = ieee.numeric_bit.signed'("101"));
        check_relation(decode_numeric_bit_signed(encode_numeric_bit_signed("1")) = ieee.numeric_bit.signed'("1"));
        check_relation(decode_numeric_bit_signed(encode_numeric_bit_signed("")) = ieee.numeric_bit.signed'(""));
        check_relation(decode_numeric_bit_signed(encode_numeric_bit_signed((5 downto 3 => "101"))) = ieee.numeric_bit.signed'("101"));
        check_relation(decode_numeric_bit_signed(encode_numeric_bit_signed((5 downto 3 => "101")))'left = 5);
        check_relation(decode_numeric_bit_signed(encode_numeric_bit_signed((5 downto 3 => "101")))'right = 3);
      elsif run("Test that unsigned from numeric_std can be encoded and decoded") then
        check_relation(decode_numeric_std_unsigned(encode_numeric_std_unsigned("101")) = ieee.numeric_std.unsigned'("101"));
        check_relation(decode_numeric_std_unsigned(encode_numeric_std_unsigned("1")) = ieee.numeric_std.unsigned'("1"));
        check_relation(decode_numeric_std_unsigned(encode_numeric_std_unsigned("")) = ieee.numeric_std.unsigned'(""));
        check_relation(decode_numeric_std_unsigned(encode_numeric_std_unsigned((5 downto 3 => "101"))) = ieee.numeric_std.unsigned'("101"));
        check_relation(decode_numeric_std_unsigned(encode_numeric_std_unsigned((5 downto 3 => "101")))'left = 5);
        check_relation(decode_numeric_std_unsigned(encode_numeric_std_unsigned((5 downto 3 => "101")))'right = 3);
      elsif run("Test that signed from numeric_std can be encoded and decoded") then
        check_relation(decode_numeric_std_signed(encode_numeric_std_signed("101")) = ieee.numeric_std.signed'("101"));
        check_relation(decode_numeric_std_signed(encode_numeric_std_signed("1")) = ieee.numeric_std.signed'("1"));
        check_relation(decode_numeric_std_signed(encode_numeric_std_signed("")) = ieee.numeric_std.signed'(""));
        check_relation(decode_numeric_std_signed(encode_numeric_std_signed((5 downto 3 => "101"))) = ieee.numeric_std.signed'("101"));
        check_relation(decode_numeric_std_signed(encode_numeric_std_signed((5 downto 3 => "101")))'left = 5);
        check_relation(decode_numeric_std_signed(encode_numeric_std_signed((5 downto 3 => "101")))'right = 3);
      elsif run("Test that ufixed can be encoded and decoded") then
        check_relation(decode_ufixed(encode_ufixed(to_ufixed(6.5, 3, -3))) = to_ufixed(6.5, 3, -3));
        check_relation(decode_ufixed(encode_ufixed(to_ufixed(8.0, 3, 1))) = to_ufixed(8.0, 3, 1));
        check_relation(decode_ufixed(encode_ufixed(to_ufixed(0.25, -2, -4))) = to_ufixed(0.25, -2, -4));
      elsif run("Test that sfixed can be encoded and decoded") then
        check_relation(decode_sfixed(encode_sfixed(to_sfixed(6.5, 3, -3))) = to_sfixed(6.5, 3, -3));
        check_relation(decode_sfixed(encode_sfixed(to_sfixed(8.0, 4, 1))) = to_sfixed(8.0, 4, 1));
        check_relation(decode_sfixed(encode_sfixed(to_sfixed(0.25, -1, -4))) = to_sfixed(0.25, -1, -4));
        check_relation(decode_sfixed(encode_sfixed(to_sfixed(-6.5, 3, -3))) = to_sfixed(-6.5, 3, -3));
        check_relation(decode_sfixed(encode_sfixed(to_sfixed(-8.0, 4, 1))) = to_sfixed(-8.0, 4, 1));
        check_relation(decode_sfixed(encode_sfixed(to_sfixed(-0.25, -1, -4))) = to_sfixed(-0.25, -1, -4));
      elsif run("Test that float can be encoded and decoded") then
        check_relation(decode_float(encode_float(to_float(real'low, 11, 52))) = to_float(real'low, 11, 52));
        check_relation(decode_float(encode_float(to_float(real'high, 11, 52))) = to_float(real'high, 11, 52));

        check_relation(to_string(decode_float(encode_float(positive_zero))) = to_string(positive_zero));
        check_relation(to_string(decode_float(encode_float(negative_zero))) = to_string(negative_zero));
        check_relation(to_string(decode_float(encode_float(positive_infinity))) = to_string(positive_infinity));
        check_relation(to_string(decode_float(encode_float(negative_infinity))) = to_string(negative_infinity));
        check_relation(to_string(decode_float(encode_float(nan))) = to_string(nan));
        check_relation(to_string(decode_float(encode_float(nan))) /= to_string(positive_zero));
        check_relation(to_string(decode_float(encode_float(negative_zero))) /= to_string(positive_zero));
      elsif run("Test that custom enumeration type can be encoded and decoded") then
        check_relation(decode_enum1_t(encode_enum1_t(red)) = red);
        check_relation(decode_enum1_t(encode_enum1_t(green)) = green);
        check_relation(decode_enum1_t(encode_enum1_t(blue)) = blue);
      elsif run("Test that custom record type can be encoded and decoded") then
        check_relation(decode_record1_t(encode_record1_t((character'pos(lp), -1, -2, -3))) = record1_t'((character'pos(lp), -1, -2, -3)));
        check_relation(decode_record2_t(encode_record2_t((command, 1, -1, -2, -3, '1'))) = record2_t'((command, 1, -1, -2, -3, '1')));
        check_relation(decode_record2_t(command(1, -1, -2, -3, '1')) = record2_t'((command, 1, -1, -2, -3, '1')));
        check_relation(decode_record3_t(encode_record3_t((char => comma))) = record3_t'((char => comma)));
        check_relation(decode_record3_t(encode_record3_t((char => lp))) = record3_t'((char => lp)));
        check_relation(decode_record3_t(encode_record3_t((char => rp))) = record3_t'((char => rp)));
      elsif run("Test that custom array can be encoded and decoded") then
        check_relation(decode_array1_t(encode_array1_t((0, 1, 2, 3, 4))) = array1_t'((0, 1, 2, 3, 4)));
        check_relation(decode_array1_t(encode_array1_t((0, 1, 2, 3, 4)))'left = -2);
        check_relation(decode_array1_t(encode_array1_t((0, 1, 2, 3, 4)))'right = 2);
        check_relation(decode_array2_t(encode_array2_t((0, 1, 2, 3, 4))) = array2_t'((0, 1, 2, 3, 4)));
        check_relation(decode_array2_t(encode_array2_t((0, 1, 2, 3, 4)))'left = 2);
        check_relation(decode_array2_t(encode_array2_t((0, 1, 2, 3, 4)))'right = -2);
        check_relation(decode_array3_t(encode_array3_t(((0, 1, 2), (3, 4, 5), (6, 7, 8), (9, 10, 11), (12, 13, 14)))) =
                       array3_t'(((0, 1, 2), (3, 4, 5), (6, 7, 8), (9, 10, 11), (12, 13, 14))));
        check_relation(decode_array3_t(encode_array3_t(((0, 1, 2), (3, 4, 5), (6, 7, 8), (9, 10, 11), (12, 13, 14))))'left(1) = -2);
        check_relation(decode_array3_t(encode_array3_t(((0, 1, 2), (3, 4, 5), (6, 7, 8), (9, 10, 11), (12, 13, 14))))'right(1) = 2);
        check_relation(decode_array3_t(encode_array3_t(((0, 1, 2), (3, 4, 5), (6, 7, 8), (9, 10, 11), (12, 13, 14))))'left(2) = -1);
        check_relation(decode_array3_t(encode_array3_t(((0, 1, 2), (3, 4, 5), (6, 7, 8), (9, 10, 11), (12, 13, 14))))'right(2) = 1);
        check_relation(decode_array4_t(encode_array4_t(null_array4_t)) = null_array4_t);
        check_relation(decode_array4_t(encode_array4_t((0, 1, 2, 3, 4))) = array4_t'((0, 1, 2, 3, 4)));
        check_relation(decode_array4_t(encode_array4_t((0, 1, 2, 3, 4, 5))) = array4_t'((0, 1, 2, 3, 4, 5)));
        check_relation(decode_array5_t(encode_array5_t(null_array5_t)) = null_array5_t);
        check_relation(decode_array5_t(encode_array5_t(null_array5_2_t)) = null_array5_2_t);
        check_relation(decode_array5_t(encode_array5_t(null_array5_3_t)) = null_array5_3_t);
        check_relation(decode_array5_t(encode_array5_t(((0, 1, 2), (3, 4, 5), (6, 7, 8), (9, 10, 11), (12, 13, 14)))) = array5_t'(((0, 1, 2), (3, 4, 5), (6, 7, 8), (9, 10, 11), (12, 13, 14))));
        check_relation(decode_array5_t(encode_array5_t(((0, 1, 2), (3, 4, 5), (6, 7, 8), (9, 10, 11), (12, 13, 14), (15, 16, 17)))) = array5_t'(((0, 1, 2), (3, 4, 5), (6, 7, 8), (9, 10, 11), (12, 13, 14), (15, 16, 17))));
        check_relation(decode_array6_t(encode_array6_t(null_array6_t)) = null_array6_t);
        check_relation(decode_array6_t(encode_array6_t((0, 1, 2, 3, 4))) = array6_t'((0, 1, 2, 3, 4)));
        check_relation(decode_array6_t(encode_array6_t((0, 1, 2, 3, 4, 5))) = array6_t'((0, 1, 2, 3, 4, 5)));
        check_relation(decode_array7_t(encode_array7_t(null_array7_t)) = null_array7_t);
        check_relation(decode_array7_t(encode_array7_t(((0, 1, 2), (3, 4, 5), (6, 7, 8), (9, 10, 11), (12, 13, 14)))) = array7_t'(((0, 1, 2), (3, 4, 5), (6, 7, 8), (9, 10, 11), (12, 13, 14))));
        check_relation(decode_array7_t(encode_array7_t(((0, 1, 2), (3, 4, 5), (6, 7, 8), (9, 10, 11), (12, 13, 14), (15, 16, 17)))) = array7_t'(((0, 1, 2), (3, 4, 5), (6, 7, 8), (9, 10, 11), (12, 13, 14), (15, 16, 17))));
        check_relation(decode_array8_t(encode_array8_t(((0, 1, 2), (3, 4, 5), (6, 7, 8), (9, 10, 11), (12, 13, 14)))) =
                       array8_t'(((0, 1, 2), (3, 4, 5), (6, 7, 8), (9, 10, 11), (12, 13, 14))));
        check_relation(decode_array8_t(encode_array8_t(((0, 1, 2), (3, 4, 5), (6, 7, 8), (9, 10, 11), (12, 13, 14))))'left(1) = -2);
        check_relation(decode_array8_t(encode_array8_t(((0, 1, 2), (3, 4, 5), (6, 7, 8), (9, 10, 11), (12, 13, 14))))'right(1) = 2);
        check_relation(decode_array8_t(encode_array8_t(((0, 1, 2), (3, 4, 5), (6, 7, 8), (9, 10, 11), (12, 13, 14))))'left(2) = -1);
        check_relation(decode_array8_t(encode_array8_t(((0, 1, 2), (3, 4, 5), (6, 7, 8), (9, 10, 11), (12, 13, 14))))'right(2) = 1);
        check_relation(decode_array9_t(encode_array9_t((0, 1, 2, 3, 4))) = array9_t'((0, 1, 2, 3, 4)));
        check_relation(decode_array9_t(encode_array9_t((0, 1, 2, 3, 4)))'left = -2);
        check_relation(decode_array9_t(encode_array9_t((0, 1, 2, 3, 4)))'right = 2);
        check_relation(decode_array10_t(encode_array10_t(((0, 1, 2), (3, 4, 5), (6, 7, 8), (9, 10, 11), (12, 13, 14)))) =
                       array10_t'(((0, 1, 2), (3, 4, 5), (6, 7, 8), (9, 10, 11), (12, 13, 14))));
        check_relation(decode_array10_t(encode_array10_t(((0, 1, 2), (3, 4, 5), (6, 7, 8), (9, 10, 11), (12, 13, 14))))'left(1) = -2);
        check_relation(decode_array10_t(encode_array10_t(((0, 1, 2), (3, 4, 5), (6, 7, 8), (9, 10, 11), (12, 13, 14))))'right(1) = 2);
        check_relation(decode_array10_t(encode_array10_t(((0, 1, 2), (3, 4, 5), (6, 7, 8), (9, 10, 11), (12, 13, 14))))'left(2) = -1);
        check_relation(decode_array10_t(encode_array10_t(((0, 1, 2), (3, 4, 5), (6, 7, 8), (9, 10, 11), (12, 13, 14))))'right(2) = 1);
      elsif run("Test that all provided codecs can be used within a composite") then
        my_record4 := (17, 42.21, -365 ns, true, '0', 'U', error, open_ok, read_mode, 21);
        check_relation(decode_record4_t(encode_record4_t(my_record4)) = record4_t'(my_record4));

        my_record5 := ('f', "abc", (true, false, false), ('1', '0', '0'), (17, 21, 42), (-3.14, 2.71, 1000.1000), (-13 ns, 14 ps, 3 ms), "1UX", (1.12, -0.25), 'g');
        check_relation(decode_record5_t(encode_record5_t(my_record5)) = record5_t'(my_record5));

        my_record6 := ((112.3, 0.48), "100", "011", "1XU", "LHW", "1U0X1", "01LZ1", to_float(234.56, f64), (12.3, -0.48));
        check_relation(decode_record6_t(encode_record6_t(my_record6)) = record6_t'(my_record6));

        my_record7 := (my_record4, my_record5, my_record6);
        check_relation(decode_record7_t(encode_record7_t(my_record7)) = record7_t'(my_record7));
      elsif run("Test that the values of different enumeration types used for msg_type record elements get different encodings") then
        write(e1, encode(record2_msg_type_t'(command)));
        write(e2, encode(record8_msg_type_t'(read)));
        write(e3, encode(record8_msg_type_t'(write)));
        check_relation(e1.all /= e2.all);
        check_relation(e1.all /= e3.all);
        check_relation(e2.all /= e3.all);
        deallocate(e1);
        deallocate(e2);
        deallocate(e3);
      elsif run("Test that records with different msg_type enumeration types can classified with a single get_msg_type function") then
        write(e1, encode(record2_msg_type_t'(command)));
        write(e2, encode(record8_msg_type_t'(read)));
        write(e3, encode(record8_msg_type_t'(write)));
        check(get_record2_msg_type_t(e1.all) = command);
        check(get_record8_msg_type_t(e2.all) = read);
        check(get_record8_msg_type_t(e3.all) = write);
        check(get_msg_type(e1.all) = command);
        check(get_msg_type(e2.all) = read);
        check(get_msg_type(e3.all) = write);
        deallocate(e1);
        deallocate(e2);
        deallocate(e3);
      end if;
    end loop;

    test_runner_cleanup(runner);
    wait;
  end process;

  test_runner_watchdog(runner, 100 ms);
end test_fixture;

-- vunit_pragma run_all_in_same_sim