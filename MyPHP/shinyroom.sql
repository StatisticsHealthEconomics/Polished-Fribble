-- phpMyAdmin SQL Dump
-- version 4.4.15.2
-- http://www.phpmyadmin.net
--
-- Host: localhost
-- Generation Time: Sep 08, 2020 at 11:27 AM
-- Server version: 5.5.56-MariaDB
-- PHP Version: 5.4.16

SET SQL_MODE = "NO_AUTO_VALUE_ON_ZERO";
SET time_zone = "+00:00";


/*!40101 SET @OLD_CHARACTER_SET_CLIENT=@@CHARACTER_SET_CLIENT */;
/*!40101 SET @OLD_CHARACTER_SET_RESULTS=@@CHARACTER_SET_RESULTS */;
/*!40101 SET @OLD_COLLATION_CONNECTION=@@COLLATION_CONNECTION */;
/*!40101 SET NAMES utf8mb4 */;

--
-- Database: `shinyroom`
--

-- --------------------------------------------------------

--
-- Table structure for table `individual_information`
--

CREATE TABLE IF NOT EXISTS `individual_information` (
  `UserName` varchar(2) NOT NULL,
  `Password` varchar(2) NOT NULL,
  `RoomNumber` varchar(2) NOT NULL,
  `Permissions` varchar(2) NOT NULL
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

-- --------------------------------------------------------

--
-- Table structure for table `new_room_status`
--

CREATE TABLE IF NOT EXISTS `new_room_status` (
  `Date` date NOT NULL,
  `Weekday` varchar(10) NOT NULL,
  `Room_no` varchar(3) NOT NULL,
  `9am_10am` varchar(15) NOT NULL,
  `10am_11am` varchar(15) NOT NULL,
  `11am_12pm` varchar(15) NOT NULL,
  `12pm_1pm` varchar(15) NOT NULL,
  `1pm_2pm` varchar(15) NOT NULL,
  `2pm_3pm` varchar(15) NOT NULL,
  `3pm_4pm` varchar(15) NOT NULL,
  `4pm_5pm` varchar(15) NOT NULL
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

-- --------------------------------------------------------

--
-- Table structure for table `room_booked`
--

CREATE TABLE IF NOT EXISTS `room_booked` (
  `booking_no` int(5) NOT NULL,
  `date` date NOT NULL,
  `time` varchar(9) NOT NULL,
  `room_no` varchar(3) NOT NULL,
  `booker` varchar(15) NOT NULL
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

/*!40101 SET CHARACTER_SET_CLIENT=@OLD_CHARACTER_SET_CLIENT */;
/*!40101 SET CHARACTER_SET_RESULTS=@OLD_CHARACTER_SET_RESULTS */;
/*!40101 SET COLLATION_CONNECTION=@OLD_COLLATION_CONNECTION */;
