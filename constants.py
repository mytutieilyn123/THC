#!/usr/bin/env python3
"""
Unit Conversion Tool for Numerical Relativity
-------------------------------------------
Created: 2025-03-08 00:41:16 UTC
Author: mytutieilyn123
Environment: WSL Ubuntu, Python 3.12.3

Comprehensive unit conversion system between:
- SI units (mks: meters, kilograms, seconds)
- CGS units (centimeters, grams, seconds)
- Augmented Geometric units (G = c = M_sun = 1)

#################################
# System Requirements:
# - Python >= 3.12.3
# - NumPy
# - WSL or Unix-like environment
#################################

#################################
# Acronym and Unit Definitions:
#################################
# Physical Constants:
# G     - Gravitational constant
# c     - Speed of light in vacuum
# AMU   - Atomic mass unit
# H     - Planck's constant
# HBAR  - Reduced Planck's constant (h/2π)
# K     - Boltzmann constant
#
# Astronomical Constants:
# MSUN  - Mass of the Sun
# RSUN  - Radius of the Sun
# LSUN  - Luminosity of the Sun
# TSUN  - Surface temperature of the Sun
# M_EARTH - Mass of the Earth
# R_EARTH - Radius of the Earth
# AU    - Astronomical Unit (mean Earth-Sun distance)
# PC    - Parsec (parallax of 1 arcsecond)
# LYR   - Light Year (distance light travels in one year)
#
# Unit Systems:
# SI (MKS):
# m     - meter (length)
# kg    - kilogram (mass)
# s     - second (time)
# N     - Newton (force = kg⋅m/s²)
# J     - Joule (energy = N⋅m)
# W     - Watt (power = J/s)
# Pa    - Pascal (pressure = N/m²)
# K     - Kelvin (temperature)
#
# CGS:
# cm    - centimeter (length = 0.01 m)
# g     - gram (mass = 0.001 kg)
# s     - second (time)
# dyne  - dyne (force = g⋅cm/s²)
# erg   - erg (energy = dyne⋅cm)
# Ba    - Barye (pressure = dyne/cm²)
#
# Additional Units:
# eV    - Electron Volt (energy)
#
# Augmented Geometric Units (AGEO):
# All quantities expressed in dimensionless units where:
# G = c = M_sun = 1
# This creates a natural unit system for numerical relativity
#
#Additional High Energy Physics Units:
# kT    - Thermal energy (Boltzmann constant * Temperature)
# MeV   - Mega electron volt (1 MeV = 1.60217663e-13 Joules)
# Temperature conversions available:
# - Kelvin (K)
# - kT in MeV (common in high energy astrophysics)
# - kT in ergs (CGS thermal energy)
#################################

DERIVATION OF UNIT CONVERSIONS:
------------------------------
1. Fundamental Basis:
   Starting from Einstein's Field Equations: Gμν = (8πG/c⁴)Tμν
   
   The Schwarzschild metric provides natural length scales:
   ds² = -(1 - 2GM/rc²)c²dt² + (1 - 2GM/rc²)⁻¹dr² + r²(dθ² + sin²θ dφ²)

2. Natural Unit Derivations:

   a) Length Scale (L):
      - From Schwarzschild radius: rs = 2GM/c²
      - Natural unit: L = GM/c²
      - Dimensional analysis: [L] = [G][M]/[c²] = [m³/(kg·s²)][kg][s²/m²] = [m]
      - Numerical value: L = 1.47684983e3 m

   b) Time Scale (T):
      - Light-crossing time of length scale: T = L/c
      - Therefore: T = GM/c³
      - Dimensional analysis: [T] = [G][M]/[c³] = [m³/(kg·s²)][kg][s³/m³] = [s]
      - Numerical value: T = 4.92624076e-3 s

   c) Mass Scale (M):
      - Reference mass: M = M_sun
      - Numerical value: M = 1.98855e30 kg

3. Derived Unit Scales:

   a) Density (ρ):
      ρ = M/L³ = M_sun/(GM_sun/c²)³ = c⁶/(G³M_sun²)
      
   b) Pressure (P):
      P = Energy/Volume = (M_sun·c²)/(GM_sun/c²)³ = c⁸/(G²M_sun²)
      
   c) Energy (E):
      E = mc² = M_sun·c²
      
   d) Power (P):
      P = E/T = (M_sun·c²)/(GM_sun/c³) = M_sun·c⁵/G

   e) Temperature (T):
      - Temperature remains in Kelvin for all unit systems
      - Reasoning: Temperature is fundamentally a measure of average kinetic energy
      - Relationship: E = (3/2)kT where k is Boltzmann constant
      - In AGEO units: k = 7.7229851e-71 K⁻¹
      
   f) Temperature as Thermal Energy (kT):
      - Convert temperature to energy via Boltzmann constant
      - kT in SI: E = kT [Joules]
      - kT in CGS: E = kT [ergs]
      - kT in MeV: E = kT × (1.160451812e4) [MeV]
      - Conversion factor k_B = 8.617333262e-11 MeV/K
      - Physical meaning: Average energy per degree of freedom

   Physical Examples for Temperature:
   - CMB temperature: 2.725 K
   - Solar core: ~1.57e7 K ≈ 1.35 keV
   - Neutron star surface: ~6e5 K ≈ 52 eV
   - Nuclear fusion threshold: ~1e8 K ≈ 8.6 keV
   - GUT scale: ~1e28 K ≈ 10¹⁵ GeV
#################################
"""

import numpy as np
import argparse
from dataclasses import dataclass
from typing import Dict, Any, List, Tuple, Optional, Union, TypeVar, Generic
import logging
from datetime import datetime
import sys
from pathlib import Path
import warnings
from numbers import Number
import math
from functools import lru_cache, wraps

# Setup logging with current timestamp
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(name)s - %(levelname)s - %(message)s',
    handlers=[
        logging.FileHandler(f'unit_conversions_{datetime.utcnow().strftime("%Y%m%d")}.log'),
        logging.StreamHandler(sys.stdout)
    ]
)

logger = logging.getLogger(__name__)

# Execution metadata
EXECUTION_INFO = {
    'timestamp': '2025-03-08 05:14:18',
    'author': 'mytutieilyn123',
    'python_version': '3.12.3',
    'environment': 'WSL Ubuntu',
    'last_validation': '2025-03-08 05:14:18'
}

# Type variables for generic type hints
T = TypeVar('T', float, np.ndarray)

class ValidationError(Exception):
    """Custom exception for validation errors with detailed messages"""
    def __init__(self, message: str, quantity_type: str = None, value: float = None):
        self.message = message
        self.quantity_type = quantity_type
        self.value = value
        super().__init__(self.full_message)
        
    @property
    def full_message(self) -> str:
        """Construct detailed error message"""
        msg = self.message
        if self.quantity_type:
            msg += f" (Quantity Type: {self.quantity_type})"
        if self.value is not None:
            msg += f" (Value: {self.value})"
        return msg
        
def validate_time_stamp(func):
    """Decorator to validate and log function execution time"""
    @wraps(func)
    def wrapper(*args, **kwargs):
        start_time = datetime.utcnow()
        try:
            result = func(*args, **kwargs)
            end_time = datetime.utcnow()
            execution_time = (end_time - start_time).total_seconds()
            
            # Log execution time if it exceeds 0.1 seconds
            if execution_time > 0.1:
                logger.warning(f"Long execution time in {func.__name__}: {execution_time:.3f}s")
            
            return result
            
        except Exception as e:
            logger.error(f"Error in {func.__name__}: {str(e)}")
            raise
    
    return wrapper

class NumericalRelativityConstraints:
    """Additional constraints specific to numerical relativity"""
    
    # ADM quantities
    MIN_ADM_MASS = 0.0
    MAX_SPIN_PARAMETER = 1.0  # Kerr bound |a| ≤ M
    MIN_HORIZON_AREA = 0.0    # Second law of black hole mechanics
    
    # Numerical stability
    MAX_LORENTZ_FACTOR = 1e6
    MIN_LAPSE = 1e-8
    MAX_LAPSE = 1.0
    MIN_SHIFT = -1e6
    MAX_SHIFT = 1e6
    
    # Grid parameters
    MIN_RESOLUTION = 1e-3
    MAX_COURANT = 0.5
    
    @classmethod
    def validate_adm_quantities(cls, mass: float, spin: float = None) -> bool:
        """Validate ADM mass and spin parameter"""
        if mass < cls.MIN_ADM_MASS:
            raise ValidationError("ADM mass must be positive", "mass", mass)
            
        if spin is not None:
            if abs(spin) > mass:  # Kerr bound
                raise ValidationError("Spin parameter exceeds Kerr bound", "spin", spin)
        
        return True
    
    @classmethod
    def validate_numerical_parameters(cls, lapse: float = None, shift: float = None) -> bool:
        """Validate numerical evolution parameters"""
        if lapse is not None:
            if not (cls.MIN_LAPSE <= lapse <= cls.MAX_LAPSE):
                raise ValidationError("Lapse function out of bounds", "lapse", lapse)
                
        if shift is not None:
            if not (cls.MIN_SHIFT <= shift <= cls.MAX_SHIFT):
                raise ValidationError("Shift vector component out of bounds", "shift", shift)
        
        return True

class QuantityValidator:
    """Validates physical quantities and their conversions"""
    
    def __init__(self):
        self.constraints = PhysicalConstraints()
        self.nr_constraints = NumericalRelativityConstraints()
        self.dim_analyzer = DimensionalAnalysis()
        
    @validate_time_stamp
    def validate_quantity(self, value: Union[float, np.ndarray], 
                         quantity_type: str, 
                         system: str) -> bool:
        """Complete validation of a physical quantity"""
        try:
            # Basic type checking
            if not isinstance(value, (int, float, np.number, np.ndarray)):
                raise ValidationError("Invalid value type", quantity_type, value)
            
            # Convert numpy arrays to float for validation
            if isinstance(value, np.ndarray):
                if value.size == 1:
                    value = float(value)
                else:
                    return all(self.validate_quantity(v, quantity_type, system) 
                             for v in value.flat)
            
            # Physical constraints
            PhysicalConstraints.validate_physical_constraints(value, quantity_type)
            
            # Dimensional analysis
            DimensionalAnalysis.validate_dimensions(quantity_type, value)
            
            # System-specific validation
            if system == 'AGEO':
                self._validate_ageo_quantity(value, quantity_type)
            
            return True
            
        except Exception as e:
            logger.error(f"Validation error for {quantity_type}: {str(e)}")
            raise ValidationError(str(e), quantity_type, value)
    
    def _validate_ageo_quantity(self, value: float, quantity_type: str) -> bool:
        """Additional validation for AGEO quantities"""
        # Check dimensionless quantities are of order unity
        if quantity_type in ['mass', 'length', 'time']:
            if abs(value) > 1e3:
                warnings.warn(f"Large dimensionless value in AGEO units: {value}")
        return True

class ConversionValidator:
    """Validates unit conversions"""
    
    @staticmethod
    @lru_cache(maxsize=128)
    def validate_conversion_path(from_system: str, to_system: str, 
                               quantity_type: str) -> bool:
        """Validate conversion path between unit systems"""
        valid_systems = {'SI', 'CGS', 'AGEO'}
        if from_system not in valid_systems or to_system not in valid_systems:
            raise ValidationError(f"Invalid unit system: {from_system} -> {to_system}")
            
        # Validate quantity type is supported
        if quantity_type not in DimensionalAnalysis.DIMENSIONS:
            raise ValidationError(f"Unsupported quantity type: {quantity_type}")
        
        return True
    
    @staticmethod
    def validate_conversion_result(original: float, converted: float, 
                                 quantity_type: str) -> bool:
        """Validate conversion results"""
        # Check for NaN or Inf
        if np.isnan(converted) or np.isinf(converted):
            raise ValidationError("Invalid conversion result", quantity_type, converted)
            
        # Check for excessive loss of precision
        if original != 0 and converted != 0:
            relative_error = abs((original - converted)/original)
            if relative_error > 1e-10:
                warnings.warn(f"Large relative error in conversion: {relative_error}")
        
        return True        

@dataclass
class Constants:
    """Physical and astronomical constants in different unit systems"""
    
    # SI (mks) constants
    SI_G: float = 6.67384e-11      # m^3 kg^-1 s^-2
    SI_C: float = 299792458.0      # m s^-1
    SI_AMU: float = 1.660538921e-27  # kg
    SI_H: float = 6.62606957e-34    # J s
    SI_HBAR: float = 1.054571726e-34  # J s
    SI_K: float = 1.3806488e-23     # J K^-1
    
    # Astronomical constants (SI)
    SI_MSUN: float = 1.98855e30     # kg
    SI_RSUN: float = 6.955e8        # m
    SI_LSUN: float = 3.933e26       # W
    SI_TSUN: float = 5.780e3        # K
    SI_M_EARTH: float = 5.972e24    # kg
    SI_R_EARTH: float = 6478100.0   # m
    SI_AU: float = 1.49597871e11    # m
    SI_PC: float = 3.08567758e16    # m
    SI_LYR: float = 9.4605284e15    # m
    
    # SI conversion constants
    SI_EV: float = 1.60217646e-19   # J
    SI_CM: float = 0.01             # m
    SI_GRAM: float = 0.001          # kg
    SI_ERG: float = 1.0e-7          # J
    SI_DYNE: float = 1.0e-5         # N
    SI_GRAM_PER_CM_CUBED: float = 1000.0  # kg m^-3
    SI_DYNE_PER_CM_SQUARED: float = 0.1    # Pa
    
    # CGS constants
    CGS_G: float = 6.67384e-8       # cm^3 g^-1 s^-2
    CGS_C: float = 29979245800.0    # cm s^-1
    CGS_AMU: float = 1.660538921e-24  # g
    CGS_H: float = 6.62606957e-27    # erg s
    CGS_HBAR: float = 1.054571726e-27  # erg s
    CGS_K: float = 1.3806488e-16     # erg K^-1
    
    # Astronomical constants (CGS)
    CGS_MSUN: float = 1.98855e33     # g
    CGS_RSUN: float = 6.955e10       # cm
    CGS_LSUN: float = 3.933e33       # erg s^-1
    CGS_TSUN: float = 5.780e3        # K
    CGS_M_EARTH: float = 5.972e27    # g
    CGS_R_EARTH: float = 647810000.0 # cm
    CGS_AU: float = 1.49597871e13    # cm
    CGS_PC: float = 3.08567758e18    # cm
    CGS_LYR: float = 9.4605284e17    # cm
    
    # CGS conversion constants
    CGS_EV: float = 1.60217646e-12   # erg
    CGS_M: float = 100.0             # cm
    CGS_KG: float = 1000.0           # g
    CGS_J: float = 1.0e7             # erg
    CGS_N: float = 1.0e5             # dyne
    CGS_KG_PER_M_CUBED: float = 0.001  # g cm^-3
    CGS_N_PER_M_SQUARED: float = 10.0   # ba
    
    # Additional temperature/energy conversion constants
    SI_MEV: float = 1.60217663e-13  # J MeV
    CGS_MEV: float = 1.60217663e-6  # ergs MeV
    SI_K_TO_MEV: float = 8.617333262e-11  # Conversion from K to MeV (kB in MeV/K)
    CGS_K_TO_MEV: float = SI_K_TO_MEV     # Same conversion in CGS
    
    # Augmented Geometric units (G = c = M_sun = 1)
    AGEO_G: float = 1.0
    AGEO_C: float = 1.0
    AGEO_AMU: float = 8.3505012e-58
    AGEO_K: float = 7.7229851e-71    # K^-1
    AGEO_MSUN: float = 1.0
    AGEO_RSUN: float = 470934.0
    AGEO_LSUN: float = 4.4659113e-16
    
    # AGEO conversion constants
    AGEO_EV: float = 8.9621524e-67
    AGEO_M: float = 6.77116916e-4
    AGEO_CM: float = 6.77116916e-6
    AGEO_S: float = 202994.545
    AGEO_KG: float = 5.02739933e-31
    AGEO_GRAM: float = 5.02739933e-34
    AGEO_KG_PER_M_CUBED: float = 1.6193935e-21
    AGEO_GRAM_PER_CM_CUBED: float = 1.6193935e-18
    AGEO_N_PER_M_SQUARED: float = 1.80181827e-38
    AGEO_DYNE_PER_CM_SQUARED: float = 1.80181827e-39
    AGEO_J: float = 5.59373614e-48
    AGEO_ERG: float = 5.59373614e-55
    AGEO_W: float = 1.13549741e-42
    AGEO_ERGS_PER_S: float = 1.13549741e-49
    
    def __post_init__(self):
        """Validate constants after initialization"""
        try:
            validator = UnitSystemValidator()
            if not validator.validate_constants(self):
                logger.error("Constants validation failed!")
                raise ValueError("Physical constants validation failed")
            logger.info("Constants validation successful")
        except Exception as e:
            logger.error(f"Initialization error: {str(e)}")
            raise
            
@dataclass
class Constants:
    """
    Physical and astronomical constants in different unit systems
    Last Updated: 2025-03-08 05:17:39 UTC
    """
    # SI (mks) constants - Initialize with default values
    SI_G: float = 6.67384e-11      # m^3 kg^-1 s^-2
    SI_C: float = 299792458.0      # m s^-1 (exact)
    SI_AMU: float = 1.660538921e-27  # kg
    SI_H: float = 6.62606957e-34    # J s
    SI_HBAR: float = 1.054571726e-34  # J s
    SI_K: float = 1.3806488e-23     # J K^-1
    
    # Astronomical constants (SI)
    SI_MSUN: float = 1.98855e30     # kg
    SI_RSUN: float = 6.955e8        # m
    SI_LSUN: float = 3.933e26       # W
    SI_TSUN: float = 5.780e3        # K
    SI_M_EARTH: float = 5.972e24    # kg
    SI_R_EARTH: float = 6478100.0   # m
    SI_AU: float = 1.49597871e11    # m
    SI_PC: float = 3.08567758e16    # m
    SI_LYR: float = 9.4605284e15    # m
    
    # SI conversion constants
    SI_EV: float = 1.60217646e-19   # J
    SI_CM: float = 0.01             # m
    SI_GRAM: float = 0.001          # kg
    SI_ERG: float = 1.0e-7          # J
    SI_DYNE: float = 1.0e-5         # N
    SI_GRAM_PER_CM_CUBED: float = 1000.0  # kg m^-3
    SI_DYNE_PER_CM_SQUARED: float = 0.1    # Pa
    
    # CGS constants
    CGS_G: float = 6.67384e-8       # cm^3 g^-1 s^-2
    CGS_C: float = 29979245800.0    # cm s^-1
    CGS_AMU: float = 1.660538921e-24  # g
    CGS_H: float = 6.62606957e-27    # erg s
    CGS_HBAR: float = 1.054571726e-27  # erg s
    CGS_K: float = 1.3806488e-16     # erg K^-1
    
    # Astronomical constants (CGS)
    CGS_MSUN: float = 1.98855e33     # g
    CGS_RSUN: float = 6.955e10       # cm
    CGS_LSUN: float = 3.933e33       # erg s^-1
    CGS_TSUN: float = 5.780e3        # K
    CGS_M_EARTH: float = 5.972e27    # g
    CGS_R_EARTH: float = 647810000.0 # cm
    CGS_AU: float = 1.49597871e13    # cm
    CGS_PC: float = 3.08567758e18    # cm
    CGS_LYR: float = 9.4605284e17    # cm
    
    # CGS conversion constants
    CGS_EV: float = 1.60217646e-12   # erg
    CGS_M: float = 100.0             # cm
    CGS_KG: float = 1000.0           # g
    CGS_J: float = 1.0e7             # erg
    CGS_N: float = 1.0e5             # dyne
    CGS_KG_PER_M_CUBED: float = 0.001  # g cm^-3
    CGS_N_PER_M_SQUARED: float = 10.0   # ba
    
    # Temperature/energy conversion constants
    SI_MEV: float = 1.60217663e-13  # J MeV
    CGS_MEV: float = 1.60217663e-6  # ergs MeV
    SI_K_TO_MEV: float = 8.617333262e-11  # K to MeV conversion
    CGS_K_TO_MEV: float = 8.617333262e-11  # K to MeV conversion
    
    # Augmented Geometric units (G = c = M_sun = 1)
    AGEO_G: float = 1.0
    AGEO_C: float = 1.0
    AGEO_AMU: float = 8.3505012e-58
    AGEO_K: float = 7.7229851e-71    # K^-1
    AGEO_MSUN: float = 1.0
    AGEO_RSUN: float = 470934.0
    AGEO_LSUN: float = 4.4659113e-16
    
    # AGEO conversion constants
    AGEO_EV: float = 8.9621524e-67
    AGEO_M: float = 6.77116916e-4
    AGEO_CM: float = 6.77116916e-6
    AGEO_S: float = 202994.545
    AGEO_KG: float = 5.02739933e-31
    AGEO_GRAM: float = 5.02739933e-34
    AGEO_KG_PER_M_CUBED: float = 1.6193935e-21
    AGEO_GRAM_PER_CM_CUBED: float = 1.6193935e-18
    AGEO_N_PER_M_SQUARED: float = 1.80181827e-38
    AGEO_DYNE_PER_CM_SQUARED: float = 1.80181827e-39
    AGEO_J: float = 5.59373614e-48
    AGEO_ERG: float = 5.59373614e-55
    AGEO_W: float = 1.13549741e-42
    AGEO_ERGS_PER_S: float = 1.13549741e-49

    def __post_init__(self):
        """
        Validate constants after initialization
        Validation Timestamp: 2025-03-08 05:17:39 UTC
        """
        try:
            # Validate fundamental constants
            self._validate_fundamental_constants()
            
            # Validate unit system consistency
            self._validate_unit_system_consistency()
            
            # Validate AGEO normalization
            self._validate_ageo_normalization()
            
            logger.info("Constants validation successful")
            
        except Exception as e:
            logger.error(f"Constants validation failed: {str(e)}")
            raise ValidationError(str(e))

    def _validate_fundamental_constants(self) -> bool:
        """Validate fundamental physical constants"""
        validations = [
            # Exact constants
            (self.SI_C == 299792458.0, "Speed of light must be exactly 299792458 m/s"),
            
            # High-precision constants (CODATA 2018)
            (abs(self.SI_G - 6.67430e-11) < 1e-15, "G value outside CODATA 2018 bounds"),
            (abs(self.SI_HBAR - 1.054571817e-34) < 1e-43, "ℏ value outside CODATA 2018 bounds"),
            (abs(self.SI_K - 1.380649e-23) < 1e-28, "k_B value outside CODATA 2018 bounds"),
            
            # Astronomical constants (IAU 2015)
            (abs(self.SI_MSUN - 1.98847e30) < 1e25, "Solar mass outside IAU 2015 bounds"),
            (abs(self.SI_AU - 149597870700.0) < 1, "AU outside IAU 2015 definition"),
            
            # CGS consistency
            (abs(self.CGS_C - self.SI_C * 100) < 1e-8, "CGS speed of light inconsistent"),
            (abs(self.CGS_G - self.SI_G * 1e3) < 1e-10, "CGS G inconsistent")
        ]
        
        for condition, message in validations:
            if not condition:
                raise ValidationError(message)
        
        return True

    def _validate_unit_system_consistency(self) -> bool:
        """Validate consistency between unit systems"""
        # SI-CGS conversions
        assert abs(self.SI_CM - 0.01) < 1e-10, "SI-CGS length conversion error"
        assert abs(self.SI_GRAM - 0.001) < 1e-10, "SI-CGS mass conversion error"
        assert abs(self.SI_ERG - 1e-7) < 1e-10, "SI-CGS energy conversion error"
        
        # Temperature conversion consistency
        assert abs(self.SI_K_TO_MEV - self.CGS_K_TO_MEV) < 1e-20, "Temperature conversion inconsistency"
        
        return True

    def _validate_ageo_normalization(self) -> bool:
        """Validate AGEO unit normalization"""
        assert self.AGEO_G == 1.0, "AGEO G must be exactly 1"
        assert self.AGEO_C == 1.0, "AGEO c must be exactly 1"
        assert self.AGEO_MSUN == 1.0, "AGEO M_sun must be exactly 1"
        
        return True

class UnitConverter:
    """
    Handles conversions between different unit systems
    Last Updated: 2025-03-08 05:17:39 UTC
    """
    
    def __init__(self):
        """Initialize converter with validation"""
        self.constants = Constants()
        self.quantity_validator = QuantityValidator()
        self.conversion_validator = ConversionValidator()
        
        # Initialize conversion factors
        self._initialize_conversion_factors()
        
        # Validate initialization
        self._validate_initialization()

    def _initialize_conversion_factors(self):
        """Initialize conversion factors with validation"""
        try:
            # Your existing conversion factor initialization
            self.to_ageo = {
                # ... [your existing conversion factors remain the same] ...
            }
            
            # Define conversion factors from AGEO units
            self.from_ageo = {k: {unit: 1.0/factor for unit, factor in v.items()} 
                            for k, v in self.to_ageo.items()}
            
            # Validate conversion factors
            self._validate_conversion_factors()
            
        except Exception as e:
            logger.error(f"Conversion factor initialization failed: {str(e)}")
            raise ValidationError(str(e))

    @validate_time_stamp
    def convert(self, value: Union[float, np.ndarray], 
                from_system: str, to_system: str, 
                quantity_type: str) -> Union[float, np.ndarray]:
        """
        Convert a value between unit systems with full validation
        """
        try:
            # Input validation
            self.quantity_validator.validate_quantity(value, quantity_type, from_system)
            self.conversion_validator.validate_conversion_path(from_system, to_system, quantity_type)
            
            # Convert input to numpy array
            value_array = np.array(value)
            
            # Special handling for temperature
            if quantity_type == 'temperature_kt':
                result = value_array * self.constants.SI_K_TO_MEV
                self.conversion_validator.validate_conversion_result(value, result, quantity_type)
                return result
            
            # Standard conversion through AGEO units
            if from_system != 'AGEO':
                value_array = value_array * self.to_ageo[quantity_type][from_system]
            
            if to_system != 'AGEO':
                value_array = value_array * self.from_ageo[quantity_type][to_system]
            
            # Validate result
            self.conversion_validator.validate_conversion_result(value, value_array, quantity_type)
            
            return value_array
            
        except Exception as e:
            logger.error(f"Conversion error: {str(e)}")
            raise ValidationError(str(e), quantity_type, value)    

def validate_input_args(args) -> bool:
    """Validate command line arguments"""
    try:
        # Validate systems
        if args.from_system not in ['SI', 'CGS', 'AGEO']:
            raise ValidationError(f"Invalid source system: {args.from_system}")
        if args.to_system not in ['SI', 'CGS', 'AGEO']:
            raise ValidationError(f"Invalid target system: {args.to_system}")
            
        # Validate quantity type
        valid_types = ['length', 'time', 'mass', 'density', 'pressure',
                      'energy', 'power', 'force', 'temperature',
                      'temperature_kt', 'astronomical_distance']
        if args.type not in valid_types:
            raise ValidationError(f"Invalid quantity type: {args.type}")
            
        # Validate numerical value
        if not isinstance(args.quantity, (int, float)):
            raise ValidationError(f"Invalid quantity value: {args.quantity}")
            
        return True
        
    except Exception as e:
        logger.error(f"Input validation error: {str(e)}")
        raise ValidationError(str(e))

def print_execution_info():
    """Print execution environment information"""
    print("\nExecution Information:")
    print("-" * 50)
    print(f"Timestamp: {EXECUTION_INFO['timestamp']}")
    print(f"User: {EXECUTION_INFO['author']}")
    print(f"Python Version: {EXECUTION_INFO['python_version']}")
    print(f"Environment: {EXECUTION_INFO['environment']}")
    print("-" * 50)

def main():
    """
    Main execution function with comprehensive error handling
    Last Updated: 2025-03-08 05:19:03 UTC
    """
    try:
        # Setup argument parser
        parser = argparse.ArgumentParser(
            description="Convert between SI, CGS, and Augmented Geometric units",
            epilog="Example: %(prog)s --from SI --to AGEO --quantity 1.0 --type mass"
        )
        
        # Add arguments with enhanced help messages
        parser.add_argument("--from", dest="from_system",
                          choices=['SI', 'CGS', 'AGEO'],
                          required=True,
                          help="Original unit system (SI, CGS, or AGEO)")
        
        parser.add_argument("--to", dest="to_system",
                          choices=['SI', 'CGS', 'AGEO'],
                          required=True,
                          help="Target unit system (SI, CGS, or AGEO)")
        
        parser.add_argument("--quantity", type=float,
                          required=True,
                          help="Value to convert (must be a valid number)")
        
        parser.add_argument("--type",
                          choices=['length', 'time', 'mass', 'density',
                                 'pressure', 'energy', 'power', 'force',
                                 'temperature', 'temperature_kt',
                                 'astronomical_distance'],
                          required=True,
                          help="Type of quantity to convert")
        
        # Parse arguments
        args = parser.parse_args()
        
        # Validate input arguments
        validate_input_args(args)
        
        # Print execution information
        print_execution_info()
        
        # Initialize converter
        converter = UnitConverter()
        
        # Perform conversion
        try:
            result = converter.convert(
                args.quantity,
                args.from_system,
                args.to_system,
                args.type
            )
            
            # Print results
            converter.print_unit_info(
                args.type,
                args.quantity,
                args.from_system,
                args.to_system
            )
            
            # Log successful conversion
            logger.info(f"Successful conversion: {args.quantity} {args.type} "
                      f"from {args.from_system} to {args.to_system}")
            
        except Exception as e:
            logger.error(f"Conversion error: {str(e)}")
            print(f"\nError during conversion: {str(e)}")
            sys.exit(1)
            
    except Exception as e:
        logger.error(f"Program error: {str(e)}")
        print(f"\nError: {str(e)}")
        sys.exit(1)

if __name__ == "__main__":
    # Set execution timestamp
    EXECUTION_INFO.update({
        'timestamp': '2025-03-08 05:19:03',
        'author': 'mytutieilyn123'
    })
    
    try:
        main()
    except KeyboardInterrupt:
        print("\nOperation cancelled by user")
        sys.exit(0)
    except Exception as e:
        logger.critical(f"Unhandled exception: {str(e)}")
        print(f"\nCritical error: {str(e)}")
        sys.exit(1)            

class UnitConverter:
    """Handles conversions between different unit systems"""
    
    def __init__(self):
        self.constants = Constants()
        self.validator = DimensionalValidator()
        
        # Define conversion factors to AGEO units
        self.to_ageo = {
            'length': {
                'SI': 1.0 / 1.47684983e3,    # m to AGEO
                'CGS': 1.0 / 1.47684983e5     # cm to AGEO
            },
            'time': {
                'SI': 202994.545,    # s to AGEO
                'CGS': 202994.545    # s to AGEO
            },
            'mass': {
                'SI': 1.0 / self.constants.SI_MSUN,   # kg to AGEO
                'CGS': 1.0 / self.constants.CGS_MSUN  # g to AGEO
            },
            'density': {
                'SI': 1.0 / 6.17515138e20,    # kg/m^3 to AGEO
                'CGS': 1.0 / 6.17515138e17    # g/cm^3 to AGEO
            },
            'pressure': {
                'SI': 1.0 / 5.54994928e37,    # Pa to AGEO
                'CGS': 1.0 / 5.54994928e38    # dyne/cm^2 to AGEO
            },
            'energy': {
                'SI': 1.0 / 1.78771393e47,    # J to AGEO
                'CGS': 1.0 / 1.78771393e54    # erg to AGEO
            },
            'power': {
                'SI': 1.0 / 8.80671314e41,    # W to AGEO
                'CGS': 1.0 / 8.80671314e48    # erg/s to AGEO
            },
            'force': {
                'SI': 1.0 / (self.constants.SI_MSUN * self.constants.SI_G / 
                            (self.constants.SI_C ** 2)),    # N to AGEO
                'CGS': 1.0 / (self.constants.CGS_MSUN * self.constants.CGS_G / 
                             (self.constants.CGS_C ** 2))   # dyne to AGEO
            },
            'temperature': {
                'SI': 1.0,    # K to AGEO (temperature remains in Kelvin)
                'CGS': 1.0    # K to AGEO (temperature remains in Kelvin)
            },
            'temperature_kt': {
                'SI': 1.0 / (self.constants.SI_K * self.constants.SI_K_TO_MEV),  # K to MeV
                'CGS': 1.0 / (self.constants.CGS_K * self.constants.CGS_K_TO_MEV)  # K to MeV
            },
            'astronomical_distance': {
                'SI': 1.0 / self.constants.SI_AU,    # AU to AGEO
                'CGS': 1.0 / self.constants.CGS_AU   # AU to CGS
            }
        }
        
        # Define conversion factors from AGEO units
        self.from_ageo = {k: {unit: 1.0/factor for unit, factor in v.items()} 
                         for k, v in self.to_ageo.items()}
        # Validate conversion factors
        try:
            system_validator = UnitSystemValidator()
            if not system_validator.validate_conversion_factors(self):
                raise ValueError("Conversion factors validation failed")
            logger.info("Conversion factors validation successful")
        except Exception as e:
            logger.error(f"Conversion setup error: {str(e)}")
            raise                 

    def convert(self, value: float, from_system: str, to_system: str, quantity_type: str) -> float:
        """
        Convert a value between unit systems.
        
        Args:
            value: The numerical value to convert (can be numpy array)
            from_system: Original unit system ('SI', 'CGS', or 'AGEO')
            to_system: Target unit system ('SI', 'CGS', or 'AGEO')
            quantity_type: Type of quantity to convert
            
        Returns:
            Converted value in target unit system
        """
        try:
            value = np.array(value)
        
            # Validate input
            if quantity_type not in self.to_ageo:
                raise ValueError(f"Unknown quantity type: {quantity_type}")
            if from_system not in ['SI', 'CGS', 'AGEO']:
                raise ValueError(f"Unknown source unit system: {from_system}")
            if to_system not in ['SI', 'CGS', 'AGEO']:
                raise ValueError(f"Unknown target unit system: {to_system}")
        
            # Special handling for kT (MeV) conversions
            if quantity_type == 'temperature_kt':
            # Convert from Kelvin to kT (MeV)
                return value * self.constants.SI_K_TO_MEV
                
            if from_system == to_system:
                return value
            
            # Convert to AGEO first if not already in AGEO
            if from_system != 'AGEO':
                value = value * self.to_ageo[quantity_type][from_system]
            
            # Convert from AGEO to target system if needed
            if to_system != 'AGEO':
                value = value * self.from_ageo[quantity_type][to_system]
            
            return value
       
        except Exception as e:
            logger.error(f"Conversion error: {str(e)}")
            raise

    def print_unit_info(self, quantity_type: str, value: float, from_system: str, to_system: str):  # Added to_system parameter
        """Print information about the units and conversions"""
        print(f"\nUnit Conversion Info for {quantity_type}:")
        print("-" * 50)
    
        systems = ['SI', 'CGS', 'AGEO']
        units = {
            'length': {'SI': 'm', 'CGS': 'cm', 'AGEO': 'geometric units'},
            'time': {'SI': 's', 'CGS': 's', 'AGEO': 'geometric units'},
            'mass': {'SI': 'kg', 'CGS': 'g', 'AGEO': 'geometric units'},
            'density': {'SI': 'kg/m³', 'CGS': 'g/cm³', 'AGEO': 'geometric units'},
            'pressure': {'SI': 'Pa', 'CGS': 'dyne/cm²', 'AGEO': 'geometric units'},
            'energy': {'SI': 'J', 'CGS': 'erg', 'AGEO': 'geometric units'},
            'power': {'SI': 'W', 'CGS': 'erg/s', 'AGEO': 'geometric units'},
            'force': {'SI': 'N', 'CGS': 'dyne', 'AGEO': 'geometric units'},
            'temperature': {'SI': 'K', 'CGS': 'K', 'AGEO': 'K'},
            'temperature_kt': {'SI': 'K', 'CGS': 'K', 'AGEO': 'K'},
            'astronomical_distance': {'SI': 'AU', 'CGS': 'AU', 'AGEO': 'geometric units'}
        }
    
        print(f"Original value: {value} {units[quantity_type][from_system]}")
        print("\nConversions:")
        if quantity_type == 'temperature_kt':
            converted = self.convert(value, from_system, to_system, quantity_type)
            print(f"→ kT: {converted:e} MeV")
        else:
            for to_sys in systems:
                if to_sys != from_system:
                    converted = self.convert(value, from_system, to_sys, quantity_type)
                    print(f"→ {to_sys}: {converted:e} {units[quantity_type][to_sys]}")

def main():
    parser = argparse.ArgumentParser(description="Convert between SI, CGS, and Augmented Geometric units")
    parser.add_argument("--from", dest="from_system", choices=['SI', 'CGS', 'AGEO'],
                      required=True, help="Original unit system")
    parser.add_argument("--to", dest="to_system", choices=['SI', 'CGS', 'AGEO'],
                      required=True, help="Target unit system")
    parser.add_argument("--quantity", type=float, required=True,
                      help="Value to convert")
    parser.add_argument("--type", choices=['length', 'time', 'mass', 'density', 'pressure',
                                         'energy', 'power', 'force', 'temperature',
                                         'temperature_kt', 'astronomical_distance'],
                      required=True, help="Type of quantity to convert")
    
    args = parser.parse_args()
    
    converter = UnitConverter()
    result = converter.convert(args.quantity, args.from_system, args.to_system, args.type)
    converter.print_unit_info(args.type, args.quantity, args.from_system, args.to_system)  # Added args.to_system

if __name__ == "__main__":
    main()