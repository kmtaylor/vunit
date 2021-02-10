# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this file,
# You can obtain one at http://mozilla.org/MPL/2.0/.
#
# Copyright (c) 2016, Lars Asplund lars.anders.asplund@gmail.com

"""
Interface for Vivado XSim simulator
"""

from __future__ import print_function
import logging
import os
from os.path import join
import shutil
import subprocess
from ..ostools import Process
from . import SimulatorInterface, StringOption, BooleanOption
from ..exceptions import CompileError
from shutil import copyfile
LOGGER = logging.getLogger(__name__)


class XSimInterface(SimulatorInterface):
    """
    Interface for Vivado xsim simulator
    """
    name = "xsim"
    executable = os.environ.get("XSIM", "xsim")

    package_users_depend_on_bodies = True
    supports_gui_flag = True

    sim_options = [
        StringOption("xsim.timescale"),
        BooleanOption("xsim.enable_glbl"),
    ]

    @classmethod
    def from_args(cls,
                  output_path,  # pylint: disable=unused-argument
                  args):
        """
        Create instance from args namespace
        """
        prefix = cls.find_prefix()
        return cls(prefix=prefix, output_path=output_path, gui=args.gui)

    @classmethod
    def find_prefix_from_path(cls):
        """
        Find first valid xsim toolchain prefix
        """
        return cls.find_toolchain(["xsim"])

    def check_tool(self, tool_name):
        if os.path.exists(os.path.join(self._prefix, tool_name + '.bat')):
            return tool_name + '.bat'
        elif os.path.exists(os.path.join(self._prefix, tool_name)):
            return tool_name
        raise Exception('Cannot find %s' % tool_name)

    def __init__(self, prefix, output_path, gui=False):
        super(XSimInterface, self).__init__(output_path, gui)
        self._prefix = prefix
        self._libraries = {}
        self._xvlog = self.check_tool('xvlog')
        self._xvhdl = self.check_tool('xvhdl')
        self._xelab = self.check_tool('xelab')
        self._vivado = self.check_tool('vivado')

    def setup_library_mapping(self, project):
        """
        Setup library mapping
        """

        for library in project.get_libraries():
            self._libraries[library.name] = library.directory

    def compile_source_file_command(self, source_file):
        """
        Returns the command to compile a single source_file
        """
        if source_file.file_type == 'vhdl':
            return self.compile_vhdl_file_command(source_file)
        elif source_file.file_type == 'verilog':
            cmd = [join(self._prefix, self._xvlog), source_file.name]
            return self.compile_verilog_file_command(source_file, cmd)
        elif source_file.file_type == 'systemverilog':
            cmd = [join(self._prefix, self._xvlog), '--sv', source_file.name]
            return self.compile_verilog_file_command(source_file, cmd)

        LOGGER.error("Unknown file type: %s", source_file.file_type)
        raise CompileError

    def libraries_command(self):
        cmd = []
        for library_name, library_path in self._libraries.items():
            if library_path:
                cmd += ["-L", '%s=%s' % (library_name, library_path)]
            else:
                cmd += ["-L", library_name]
        return cmd

    def work_library_argument(self, source_file):
        return ["-work", "%s=%s" % (source_file.library.name,
                                    source_file.library.directory)]

    def compile_vhdl_file_command(self, source_file):
        """
        Returns the command to compile a vhdl file
        """
        cmd = [join(self._prefix, self._xvhdl), source_file.name, '-2008']
        cmd += self.work_library_argument(source_file)
        cmd += self.libraries_command()
        return cmd

    def compile_verilog_file_command(self, source_file, cmd):
        """
        Returns the command to compile a vhdl file
        """
        cmd += self.work_library_argument(source_file)
        cmd += self.libraries_command()
        for include_dir in source_file.include_dirs:
            cmd += ["--include", "%s" % include_dir]
        for define_name, define_val in source_file.defines.items():
            cmd += ["--define", "%s=%s" % (define_name, define_val)]
        return cmd

    def simulate(self,
                 output_path, test_suite_name, config, elaborate_only):
        """
        Simulate with entity as top level using generics
        """
        cmd = [join(self._prefix, self._xelab)]
        cmd += ["-debug", "typical"]
        cmd += self.libraries_command()
        if not (elaborate_only or self._gui):
            cmd += ["--runall"]

        cmd += ["--notimingchecks"]
        cmd += ["--nospecify"]
        cmd += ["--nolog"]
        cmd += ["--relax"]
        cmd += ["--incr"]
        cmd += ["--sdfnowarn"]

        cmd += ["%s.%s" % (config.library_name, config.entity_name)]

        enable_glbl = config.sim_options.get(self.name + '.enable_glbl', None)
        if (enable_glbl == True):
            cmd += ["%s.%s" % (config.library_name, 'glbl')]

        timescale = config.sim_options.get(self.name + '.timescale', None)
        if timescale:
            cmd += ['-timescale', timescale]
        dirname = os.path.dirname(self._libraries[config.library_name])
        shutil.copytree(dirname, os.path.join(output_path,
                                              os.path.basename(dirname)))
        for generic_name, generic_value in config.generics.items():
            cmd += ["--generic_top", '%s=%s' % (generic_name, generic_value)]
        if not os.path.exists(output_path):
            os.makedirs(output_path)
        status = True
        print(cmd)
        try:
            resources = config.get_resources()
            for x in resources:
                file_name = os.path.basename(x)
                copyfile(x,output_path+"/"+file_name)

            proc = Process(cmd, cwd=output_path)
            proc.consume_output()
        except Process.NonZeroExitCode:
            status = False
        if self._gui:
            tcl_file = os.path.join(output_path, "xsim_startup.tcl")
            vivado_cmd = [join(self._prefix, self._vivado),
                          "-mode", "gui",
                          "-source", tcl_file]
            if not os.path.isfile(tcl_file):
                with open(tcl_file, 'w+') as xsim_startup_file:
                    xsim_startup_file.write("set_part xc7a12tcpg238-3\n")
                    if (enable_glbl == True):
                        xsim_startup_file.write("xsim " +
                                                ("%s.%s" % (config.library_name,
                                                            config.entity_name)) +
                                                            "#" + config.library_name + ".glbl" +
                                                "\nadd_wave {{/" + config.entity_name + "}} "
                                                "\nrun all"
                                                "\n")
                    else:
                        xsim_startup_file.write("xsim " +
                                                ("%s.%s" % (config.library_name,
                                                            config.entity_name)) +
                                                "\nadd_wave {{/" + config.entity_name + "}} "
                                                "\nrun all"
                                                "\n")
            print("out_path: " + str(output_path))
            print("vivado_cmd: " + str(vivado_cmd))
            try:
                subprocess.call(vivado_cmd, cwd=output_path)
            except Process.NonZeroExitCode:
                pass
            assert False

        return status
