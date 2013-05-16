# ************************************************************
# Sequel Pro SQL dump
# Version 4004
#
# http://www.sequelpro.com/
# http://code.google.com/p/sequel-pro/
#
# Host: 127.0.0.1 (MySQL 5.5.27-log)
# Database: pool
# Generation Time: 2013-05-02 10:52:34 +0000
# ************************************************************


/*!40101 SET @OLD_CHARACTER_SET_CLIENT=@@CHARACTER_SET_CLIENT */;
/*!40101 SET @OLD_CHARACTER_SET_RESULTS=@@CHARACTER_SET_RESULTS */;
/*!40101 SET @OLD_COLLATION_CONNECTION=@@COLLATION_CONNECTION */;
/*!40101 SET NAMES utf8 */;
/*!40014 SET @OLD_FOREIGN_KEY_CHECKS=@@FOREIGN_KEY_CHECKS, FOREIGN_KEY_CHECKS=0 */;
/*!40101 SET @OLD_SQL_MODE=@@SQL_MODE, SQL_MODE='NO_AUTO_VALUE_ON_ZERO' */;
/*!40111 SET @OLD_SQL_NOTES=@@SQL_NOTES, SQL_NOTES=0 */;


# Dump of table friend
# ------------------------------------------------------------

DROP TABLE IF EXISTS `friend`;

CREATE TABLE `friend` (
  `user_id` int(11) unsigned NOT NULL,
  `friend_id` int(11) unsigned NOT NULL,
  `type` tinyint(3) unsigned NOT NULL DEFAULT '0',
  UNIQUE KEY `user_id` (`user_id`,`friend_id`),
  KEY `friend_id` (`friend_id`),
  CONSTRAINT `friend_ibfk_1` FOREIGN KEY (`user_id`) REFERENCES `user` (`user_id`) ON DELETE CASCADE ON UPDATE CASCADE,
  CONSTRAINT `friend_ibfk_2` FOREIGN KEY (`friend_id`) REFERENCES `user` (`user_id`) ON DELETE CASCADE ON UPDATE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=utf8;



# Dump of table inapp_transaction
# ------------------------------------------------------------

DROP TABLE IF EXISTS `inapp_transaction`;

CREATE TABLE `inapp_transaction` (
  `transaction_id` varchar(50) NOT NULL DEFAULT '',
  `payment_platform` int(11) NOT NULL DEFAULT '0',
  `user_id` int(11) unsigned NOT NULL,
  `product_id` varchar(50) NOT NULL DEFAULT '',
  `purchase_date` datetime NOT NULL,
  `validation_date` timestamp NOT NULL DEFAULT '0000-00-00 00:00:00',
  `processed` tinyint(4) NOT NULL DEFAULT '0',
  PRIMARY KEY (`transaction_id`,`payment_platform`),
  KEY `user_id` (`user_id`),
  CONSTRAINT `inapp_transaction_ibfk_1` FOREIGN KEY (`user_id`) REFERENCES `user` (`user_id`) ON DELETE CASCADE ON UPDATE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=utf8;



# Dump of table match_history
# ------------------------------------------------------------

DROP TABLE IF EXISTS `match_history`;

CREATE TABLE `match_history` (
  `id` bigint(11) unsigned NOT NULL AUTO_INCREMENT,
  `user_a` int(11) unsigned NOT NULL,
  `user_b` int(11) unsigned NOT NULL,
  `tier` int(10) unsigned NOT NULL,
  `start` datetime NOT NULL,
  `end` datetime DEFAULT NULL,
  `end_reason` int(11) DEFAULT NULL,
  `winner` int(11) unsigned DEFAULT NULL,
  PRIMARY KEY (`id`),
  KEY `user_a` (`user_a`),
  KEY `user_b` (`user_b`),
  KEY `winner` (`winner`),
  CONSTRAINT `match_history_ibfk_1` FOREIGN KEY (`user_a`) REFERENCES `user` (`user_id`) ON DELETE CASCADE ON UPDATE CASCADE,
  CONSTRAINT `match_history_ibfk_2` FOREIGN KEY (`user_b`) REFERENCES `user` (`user_id`) ON DELETE CASCADE ON UPDATE CASCADE,
  CONSTRAINT `match_history_ibfk_3` FOREIGN KEY (`winner`) REFERENCES `user` (`user_id`) ON DELETE CASCADE ON UPDATE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=utf8;



# Dump of table push_token
# ------------------------------------------------------------

DROP TABLE IF EXISTS `push_token`;

CREATE TABLE `push_token` (
  `user_id` int(11) unsigned NOT NULL,
  `token` varchar(200) NOT NULL DEFAULT '',
  `delivery_platform` int(11) NOT NULL DEFAULT '0',
  `last_register` datetime NOT NULL,
  UNIQUE KEY `token` (`token`,`delivery_platform`),
  KEY `user_id` (`user_id`),
  CONSTRAINT `push_token_ibfk_1` FOREIGN KEY (`user_id`) REFERENCES `user` (`user_id`) ON DELETE CASCADE ON UPDATE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=utf8;



# Dump of table uid
# ------------------------------------------------------------

DROP TABLE IF EXISTS `uid`;

CREATE TABLE `uid` (
  `user_id` int(11) unsigned NOT NULL,
  `uid` varchar(64) NOT NULL DEFAULT '',
  `type` tinyint(4) NOT NULL COMMENT '0 - openuid, 1 - sha1_mac',
  UNIQUE KEY `uid` (`uid`,`type`),
  KEY `idx_uid_user_id` (`user_id`),
  CONSTRAINT `uid_ibfk_1` FOREIGN KEY (`user_id`) REFERENCES `user` (`user_id`) ON DELETE CASCADE ON UPDATE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=utf8;



# Dump of table user
# ------------------------------------------------------------

DROP TABLE IF EXISTS `user`;

CREATE TABLE `user` (
  `user_id` int(11) unsigned NOT NULL,
  `display_name` varchar(50) NOT NULL,
  `miniclip_id` varchar(50) DEFAULT NULL,
  `facebook_id` bigint(11) unsigned DEFAULT NULL,
  `needs_update` tinyint(11) NOT NULL DEFAULT '1',
  `level` int(10) unsigned NOT NULL DEFAULT '1',
  `total_winnings` int(10) unsigned NOT NULL DEFAULT '0',
  `daily_bonus_timestamp` int(10) unsigned NOT NULL DEFAULT '0',
  `bonus_timestamp` int(10) unsigned NOT NULL DEFAULT '0',
  PRIMARY KEY (`user_id`),
  UNIQUE KEY `facebook_id` (`facebook_id`),
  KEY `miniclip_id` (`miniclip_id`),
  KEY `display_name` (`display_name`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;



# Dump of table video_transaction
# ------------------------------------------------------------

DROP TABLE IF EXISTS `video_transaction`;

CREATE TABLE `video_transaction` (
  `transaction_id` varchar(64) NOT NULL DEFAULT '',
  `platform` tinyint(4) NOT NULL COMMENT '0 - flurry, 1 - adcolony',
  `user_id` int(11) unsigned NOT NULL,
  `date` datetime NOT NULL,
  `reward` int(11) unsigned NOT NULL,
  `processed` tinyint(4) unsigned NOT NULL DEFAULT '0',
  PRIMARY KEY (`transaction_id`,`platform`),
  KEY `user_id` (`user_id`),
  CONSTRAINT `video_transaction_ibfk_1` FOREIGN KEY (`user_id`) REFERENCES `user` (`user_id`) ON DELETE CASCADE ON UPDATE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=utf8;




--
-- Dumping routines (PROCEDURE) for database 'pool'
--
DELIMITER ;;

# Dump of PROCEDURE add_friend
# ------------------------------------------------------------

/*!50003 DROP PROCEDURE IF EXISTS `add_friend` */;;
/*!50003 SET SESSION SQL_MODE=""*/;;
/*!50003 CREATE*/ /*!50020 DEFINER=`devuser`@`%`*/ /*!50003 PROCEDURE `add_friend`(
	in user_id_in int unsigned,
	in friend_id_in int unsigned
)
    MODIFIES SQL DATA
begin
    declare user_not_found condition for sqlstate '99001';
    declare invalid_parameter condition for sqlstate '99010';
	declare foreign_key_violated condition for 1452;
        
    declare continue handler for foreign_key_violated
    begin
    	signal user_not_found set message_text = 'User not found';
   	end;
   	
   	if user_id_in = friend_id_in then
   		signal invalid_parameter set message_text = 'Precondition failed user_id != friend_id';
   	end if;
   	
   	insert ignore into friend (user_id, friend_id, type) values (user_id_in, friend_id_in, 0)
   	on duplicate key update type = 0;
end */;;

/*!50003 SET SESSION SQL_MODE=@OLD_SQL_MODE */;;
# Dump of PROCEDURE add_transaction
# ------------------------------------------------------------

/*!50003 DROP PROCEDURE IF EXISTS `add_transaction` */;;
/*!50003 SET SESSION SQL_MODE=""*/;;
/*!50003 CREATE*/ /*!50020 DEFINER=`devuser`@`%`*/ /*!50003 PROCEDURE `add_transaction`(
	in user_id_in int unsigned,
	in transaction_id_in bigint unsigned,
	in product_id_in varchar(50),
	in purchase_date_in int,
	in payment_platform_in int,
	in timeout_in int	
)
    MODIFIES SQL DATA
begin

    declare user_not_found condition for sqlstate '99001';
    declare transaction_exists condition for sqlstate '99004';
    declare transaction_inflight condition for sqlstate '99006';
	declare foreign_key_violated condition for 1452;
        
    declare validation_date_val datetime;
    declare now_val datetime;
    declare processed_val int;

    declare continue handler for foreign_key_violated
    begin
    	signal user_not_found set message_text = 'User not found';
   	end;
   
    select now() into now_val;
    
	insert ignore into inapp_transaction (transaction_id, user_id, payment_platform, product_id, purchase_date, validation_date) values
		(transaction_id_in, user_id_in, payment_platform_in, product_id_in, from_unixtime(purchase_date_in), now_val);
		
	if row_count() = 0 then

	  	select validation_date, processed into validation_date_val, processed_val
	  	from inapp_transaction 
	  	where transaction_id = transaction_id_in and payment_platform = payment_platform_in;
		
		if processed_val = 1 then
			signal transaction_exists set message_text = 'transaction already closed';
		else
			update inapp_transaction set validation_date = now_val
			where transaction_id = transaction_id_in and payment_platform = payment_platform_in;

			if now_val < date_add(validation_date_val, interval timeout_in second) then
				signal transaction_inflight set message_text = 'transaction being processed';
			end if;
		
		end if;

	end if;
end */;;

/*!50003 SET SESSION SQL_MODE=@OLD_SQL_MODE */;;
# Dump of PROCEDURE add_transaction2
# ------------------------------------------------------------

/*!50003 DROP PROCEDURE IF EXISTS `add_transaction2` */;;
/*!50003 SET SESSION SQL_MODE=""*/;;
/*!50003 CREATE*/ /*!50020 DEFINER=`devuser`@`%`*/ /*!50003 PROCEDURE `add_transaction2`(
	in user_id_in int unsigned,
	in transaction_id_in varchar(50),
	in product_id_in varchar(50),
	in purchase_date_in int,
	in payment_platform_in int,
	in timeout_in int	
)
    MODIFIES SQL DATA
begin

    declare user_not_found condition for sqlstate '99001';
    declare transaction_exists condition for sqlstate '99004';
    declare transaction_inflight condition for sqlstate '99006';
	declare foreign_key_violated condition for 1452;
        
    declare validation_date_val datetime;
    declare now_val datetime;
    declare processed_val int;

    declare continue handler for foreign_key_violated
    begin
    	signal user_not_found set message_text = 'User not found';
   	end;
   
    select now() into now_val;
    
	insert ignore into inapp_transaction (transaction_id, user_id, payment_platform, product_id, purchase_date, validation_date) values
		(transaction_id_in, user_id_in, payment_platform_in, product_id_in, from_unixtime(purchase_date_in), now_val);
		
	if row_count() = 0 then

	  	select validation_date, processed into validation_date_val, processed_val
	  	from inapp_transaction 
	  	where transaction_id = transaction_id_in and payment_platform = payment_platform_in;
		
		if processed_val = 1 then
			signal transaction_exists set message_text = 'transaction already closed';
		else
			update inapp_transaction set validation_date = now_val
			where transaction_id = transaction_id_in and payment_platform = payment_platform_in;

			if now_val < date_add(validation_date_val, interval timeout_in second) then
				signal transaction_inflight set message_text = 'transaction being processed';
			end if;
		
		end if;

	end if;
end */;;

/*!50003 SET SESSION SQL_MODE=@OLD_SQL_MODE */;;
# Dump of PROCEDURE add_vr_transaction
# ------------------------------------------------------------

/*!50003 DROP PROCEDURE IF EXISTS `add_vr_transaction` */;;
/*!50003 SET SESSION SQL_MODE=""*/;;
/*!50003 CREATE*/ /*!50020 DEFINER=`devuser`@`%`*/ /*!50003 PROCEDURE `add_vr_transaction`(
	in transaction_id_in varchar(64),
	in platform_in tinyint,
	in uid_in varchar(64),
	in uid_type_in tinyint,
	in reward_in int unsigned
)
    MODIFIES SQL DATA
begin
    declare user_not_found condition for sqlstate '99001';
    declare transaction_exists condition for sqlstate '99004';
	declare foreign_key_violated condition for 1452;
	declare user_id_val int unsigned;
	declare processed_val tinyint unsigned;

    declare continue handler for foreign_key_violated
    begin
    	signal user_not_found set message_text = 'User not found';
   	end;

	select user_id into user_id_val from uid where uid = lower(uid_in) and type = uid_type_in limit 1;

	insert ignore into video_transaction (transaction_id, platform, user_id, reward, date) values (
		transaction_id_in, 
		platform_in, 
		coalesce(user_id_val, 0), 
		reward_in, 
		now());

	if row_count() = 0 then
		select processed into processed_val from video_transaction where transaction_id = transaction_id_in;
		if processed_val = 1 then
			signal transaction_exists set message_text = 'transaction exists';
		end if;
	end if;
	select user_id_val as user_id, reward_in as reward;
end */;;

/*!50003 SET SESSION SQL_MODE=@OLD_SQL_MODE */;;
# Dump of PROCEDURE close_transaction
# ------------------------------------------------------------

/*!50003 DROP PROCEDURE IF EXISTS `close_transaction` */;;
/*!50003 SET SESSION SQL_MODE=""*/;;
/*!50003 CREATE*/ /*!50020 DEFINER=`devuser`@`%`*/ /*!50003 PROCEDURE `close_transaction`(
	in transaction_id_in bigint unsigned,
	in payment_platform_in int
)
    MODIFIES SQL DATA
begin

    declare transaction_not_found condition for sqlstate '99005';
        
	update inapp_transaction set processed = 1 
	where transaction_id = transaction_id_in and payment_platform = payment_platform_in;
		
	if row_count() = 0 then
		signal transaction_not_found set message_text = 'transaction not found';
	end if;
end */;;

/*!50003 SET SESSION SQL_MODE=@OLD_SQL_MODE */;;
# Dump of PROCEDURE close_transaction2
# ------------------------------------------------------------

/*!50003 DROP PROCEDURE IF EXISTS `close_transaction2` */;;
/*!50003 SET SESSION SQL_MODE=""*/;;
/*!50003 CREATE*/ /*!50020 DEFINER=`devuser`@`%`*/ /*!50003 PROCEDURE `close_transaction2`(
	in transaction_id_in varchar(50),
	in payment_platform_in int
)
    MODIFIES SQL DATA
begin

    declare transaction_not_found condition for sqlstate '99005';
        
	update inapp_transaction set processed = 1 
	where transaction_id = transaction_id_in and payment_platform = payment_platform_in;
		
	if row_count() = 0 then
		signal transaction_not_found set message_text = 'transaction not found';
	end if;
end */;;

/*!50003 SET SESSION SQL_MODE=@OLD_SQL_MODE */;;
# Dump of PROCEDURE close_vr_transaction
# ------------------------------------------------------------

/*!50003 DROP PROCEDURE IF EXISTS `close_vr_transaction` */;;
/*!50003 SET SESSION SQL_MODE=""*/;;
/*!50003 CREATE*/ /*!50020 DEFINER=`devuser`@`%`*/ /*!50003 PROCEDURE `close_vr_transaction`(
	in transaction_id_in varchar(64),
	in platform_in int
)
    MODIFIES SQL DATA
begin

    declare transaction_not_found condition for sqlstate '99005';
        
	update video_transaction set processed = 1 
	where transaction_id = transaction_id_in and platform = platform_in;
		
	if row_count() = 0 then
		signal transaction_not_found set message_text = 'transaction not found';
	end if;
end */;;

/*!50003 SET SESSION SQL_MODE=@OLD_SQL_MODE */;;
# Dump of PROCEDURE delete_friend
# ------------------------------------------------------------

/*!50003 DROP PROCEDURE IF EXISTS `delete_friend` */;;
/*!50003 SET SESSION SQL_MODE=""*/;;
/*!50003 CREATE*/ /*!50020 DEFINER=`devuser`@`%`*/ /*!50003 PROCEDURE `delete_friend`(
	in user_id_in int unsigned,
	in friend_id_in int unsigned
)
    MODIFIES SQL DATA
begin
	delete from friend where user_id = user_id_in and friend_id = frined_id_in and type = 0;
end */;;

/*!50003 SET SESSION SQL_MODE=@OLD_SQL_MODE */;;
# Dump of PROCEDURE end_match
# ------------------------------------------------------------

/*!50003 DROP PROCEDURE IF EXISTS `end_match` */;;
/*!50003 SET SESSION SQL_MODE=""*/;;
/*!50003 CREATE*/ /*!50020 DEFINER=`devuser`@`%`*/ /*!50003 PROCEDURE `end_match`(
	in match_id_in bigint unsigned,
	in end_reason_in int,
	in winner_in int unsigned
)
    MODIFIES SQL DATA
begin
    declare user_not_found condition for sqlstate '99001';
    declare match_not_found condition for sqlstate '99008';
	declare foreign_key_violated condition for 1452;
        
    declare continue handler for foreign_key_violated
    begin
    	signal user_not_found set message_text = 'User not found';
   	end;

	if winner_in = 0 then
		update match_history set end = now(), end_reason = end_reason_in where id = match_id_in and end is null;
	else
		update match_history set end = now(), end_reason = end_reason_in, winner = winner_in where id = match_id_in and end is null and (user_a = winner_in or user_b = winner_in);
	end if;

	if row_count() = 0 then
    	signal match_not_found set message_text = 'Precondition failed';
	end if;
end */;;

/*!50003 SET SESSION SQL_MODE=@OLD_SQL_MODE */;;
# Dump of PROCEDURE get_friends
# ------------------------------------------------------------

/*!50003 DROP PROCEDURE IF EXISTS `get_friends` */;;
/*!50003 SET SESSION SQL_MODE=""*/;;
/*!50003 CREATE*/ /*!50020 DEFINER=`devuser`@`%`*/ /*!50003 PROCEDURE `get_friends`(
	in user_id_in int unsigned,
	in include_fb_friends_in tinyint unsigned
)
    READS SQL DATA
begin
    declare user_not_found condition for sqlstate '99001';
    declare need_friend_update condition for sqlstate '99007';
	declare facebook_id_val bigint unsigned;
	declare needs_update_val tinyint unsigned;

	if include_fb_friends_in = 1 then
		select facebook_id, needs_update into facebook_id_val, needs_update_val from user where user_id = user_id_in;
		
		if row_count() = 0 then
			signal user_not_found set message_text = 'User not found';
		end if;

		if needs_update_val = 1 then
			signal need_friend_update set message_text = 'User needs FB friend list update';
		end if;
	
		select uf.user_id, uf.facebook_id, uf.display_name, uf.level, uf.total_winnings from user u
			inner join friend f on u.user_id = f.user_id
			inner join user uf on f.friend_id = uf.user_id
		where u.user_id = user_id_in
		order by uf.facebook_id;
	else
		select uf.user_id, uf.facebook_id, uf.display_name, uf.level, uf.total_winnings from user u
			inner join friend f on u.user_id = f.user_id and f.type = 0
			inner join user uf on f.friend_id = uf.user_id
		where u.user_id = user_id_in
		order by uf.facebook_id;
	end if;
end */;;

/*!50003 SET SESSION SQL_MODE=@OLD_SQL_MODE */;;
# Dump of PROCEDURE get_user_info
# ------------------------------------------------------------

/*!50003 DROP PROCEDURE IF EXISTS `get_user_info` */;;
/*!50003 SET SESSION SQL_MODE=""*/;;
/*!50003 CREATE*/ /*!50020 DEFINER=`devuser`@`%`*/ /*!50003 PROCEDURE `get_user_info`(
	in user_id_in int unsigned
)
    READS SQL DATA
begin
	select user_id, display_name, miniclip_id, level, total_winnings from user where user_id = user_id_in;
end */;;

/*!50003 SET SESSION SQL_MODE=@OLD_SQL_MODE */;;
# Dump of PROCEDURE get_user_tokens
# ------------------------------------------------------------

/*!50003 DROP PROCEDURE IF EXISTS `get_user_tokens` */;;
/*!50003 SET SESSION SQL_MODE=""*/;;
/*!50003 CREATE*/ /*!50020 DEFINER=`devuser`@`%`*/ /*!50003 PROCEDURE `get_user_tokens`(
	in user_id_in int unsigned,
	in max_tokens_in int
)
    READS SQL DATA
BEGIN
	select token, delivery_platform from push_token where user_id = user_id_in 
	order by last_register desc limit max_tokens_in;
END */;;

/*!50003 SET SESSION SQL_MODE=@OLD_SQL_MODE */;;
# Dump of PROCEDURE get_user_transactions
# ------------------------------------------------------------

/*!50003 DROP PROCEDURE IF EXISTS `get_user_transactions` */;;
/*!50003 SET SESSION SQL_MODE=""*/;;
/*!50003 CREATE*/ /*!50020 DEFINER=`devuser`@`%`*/ /*!50003 PROCEDURE `get_user_transactions`(
	in user_id_in int unsigned
)
    READS SQL DATA
begin
	select * from inapp_transaction where user_id = user_id_in order by purchase_date desc;
end */;;

/*!50003 SET SESSION SQL_MODE=@OLD_SQL_MODE */;;
# Dump of PROCEDURE login_user
# ------------------------------------------------------------

/*!50003 DROP PROCEDURE IF EXISTS `login_user` */;;
/*!50003 SET SESSION SQL_MODE=""*/;;
/*!50003 CREATE*/ /*!50020 DEFINER=`devuser`@`%`*/ /*!50003 PROCEDURE `login_user`(
	in user_id_in int,
	in display_name_in varchar(50),
	in facebook_id_in bigint unsigned,
	in miniclip_id_in varchar(50)
)
    MODIFIES SQL DATA
begin

	declare facebook_id_val bigint unsigned;
	declare display_name_val varchar(100);
    declare miniclip_id_val varchar(100);
	declare needs_update_val integer unsigned;
	declare bonus_timestamp_val integer unsigned;
	
	select facebook_id, display_name, miniclip_id, needs_update, bonus_timestamp
	into facebook_id_val, display_name_val, miniclip_id_val, needs_update_val, bonus_timestamp_val
	from user where user_id = user_id_in;
	
	if found_rows() = 0 then
		update user set facebook_id = null where facebook_id = facebook_id_in;
		
		if facebook_id_in = 0 then
			insert into user (user_id, display_name, needs_update) values (user_id_in, display_name_in, 0);
			select 1 as is_new_user, 0 as needs_update, 0 as bonus_timestamp;
		else
			insert into user (user_id, display_name, facebook_id, needs_update) values (user_id_in, display_name_in, facebook_id_in, 1);
			select 1 as is_new_user, 1 as needs_update, 0 as bonus_timestamp;
		end if;
	else
		if (miniclip_id_in != coalesce(miniclip_id_val, '')) then
			update user set miniclip_id = miniclip_id_in where user_id = user_id_in;
		end if;
		if facebook_id_in != 0 and coalesce(facebook_id_val,0) != facebook_id_in then 
			delete from friend where user_id = (select user_id from user where facebook_id = facebook_id_in) and type = 1;
			update user set facebook_id = null, needs_update = 0 where facebook_id = facebook_id_in;
			update user set facebook_id = facebook_id_in, needs_update = 1, display_name = display_name_in where user_id = user_id_in;
            select 0 as is_new_user, 1 as needs_update, bonus_timestamp_val as bonus_timestamp;
		else
            select 0 as is_new_user, needs_update_val as needs_update, bonus_timestamp_val as bonus_timestamp;
		end if;
	end if;
end */;;

/*!50003 SET SESSION SQL_MODE=@OLD_SQL_MODE */;;
# Dump of PROCEDURE logout_user
# ------------------------------------------------------------

/*!50003 DROP PROCEDURE IF EXISTS `logout_user` */;;
/*!50003 SET SESSION SQL_MODE=""*/;;
/*!50003 CREATE*/ /*!50020 DEFINER=`devuser`@`%`*/ /*!50003 PROCEDURE `logout_user`(
	in user_id_in int,
	in level_in int,
	in total_winnings_in int,
	in bonus_timestamp_in int unsigned
)
    MODIFIES SQL DATA
begin
	update user set level = level_in, 
		total_winnings = total_winnings_in, 
		bonus_timestamp = bonus_timestamp_in
	where user_id = user_id_in;
end */;;

/*!50003 SET SESSION SQL_MODE=@OLD_SQL_MODE */;;
# Dump of PROCEDURE mark_fb_user_for_update
# ------------------------------------------------------------

/*!50003 DROP PROCEDURE IF EXISTS `mark_fb_user_for_update` */;;
/*!50003 SET SESSION SQL_MODE=""*/;;
/*!50003 CREATE*/ /*!50020 DEFINER=`devuser`@`%`*/ /*!50003 PROCEDURE `mark_fb_user_for_update`(
	in facebook_user_id_in bigint unsigned
)
    MODIFIES SQL DATA
begin
    declare user_not_found condition for sqlstate '99001';
   	
   	update user set needs_update = 1 where facebook_id = facebook_user_id_in;

   	if row_count() > 0 then
   		delete from friend where user_id = (select u.user_id from user u where facebook_id = facebook_user_id_in) and type = 1;
   	end if;
end */;;

/*!50003 SET SESSION SQL_MODE=@OLD_SQL_MODE */;;
# Dump of PROCEDURE register_device_data
# ------------------------------------------------------------

/*!50003 DROP PROCEDURE IF EXISTS `register_device_data` */;;
/*!50003 SET SESSION SQL_MODE=""*/;;
/*!50003 CREATE*/ /*!50020 DEFINER=`devuser`@`%`*/ /*!50003 PROCEDURE `register_device_data`(
    in user_id_in int,
    in token_in VARCHAR(200),
    in delivery_platform_in int,
	in openuid_in VARCHAR(64),
    in sha1_mac VARCHAR(64)
)
    MODIFIES SQL DATA
BEGIN
	declare now_t datetime;
    declare user_not_found condition for sqlstate '99001';
    declare foreign_key_violated condition for 1452;
        
    declare continue handler for foreign_key_violated
    begin
    	signal user_not_found set message_text = 'User not found';
    end;

	if (length(token_in) > 0) then
    	select now() into now_t;
    	insert into push_token (user_id, token, last_register, delivery_platform) values (user_id_in, token_in, now_t, delivery_platform_in)
    		on duplicate key update user_id = user_id_in, last_register = now_t;
    end if;
	if (length(openuid_in) > 0) then
       	insert into uid (user_id, uid, type) values (user_id_in, openuid_in, 0)
       		on duplicate key update user_id = user_id_in;
    end if;        
    if (length(sha1_mac) > 0) then
    	insert into uid (user_id, uid, type) values (user_id_in, sha1_mac, 1)
        	on duplicate key update user_id = user_id_in;
    end if;
END */;;

/*!50003 SET SESSION SQL_MODE=@OLD_SQL_MODE */;;
# Dump of PROCEDURE register_token
# ------------------------------------------------------------

/*!50003 DROP PROCEDURE IF EXISTS `register_token` */;;
/*!50003 SET SESSION SQL_MODE=""*/;;
/*!50003 CREATE*/ /*!50020 DEFINER=`devuser`@`%`*/ /*!50003 PROCEDURE `register_token`(
        in user_id_in int,
        in token_in VARCHAR(200),
        in delivery_platform_in int
)
    MODIFIES SQL DATA
BEGIN
        declare now_t datetime;
        declare user_not_found condition for sqlstate '99001';
        declare foreign_key_violated condition for 1452;
        
        declare continue handler for foreign_key_violated
        begin
        	signal user_not_found set message_text = 'User not found';
        end;

        select now() into now_t;
        insert into push_token (user_id, token, last_register, delivery_platform) values (user_id_in, token_in, now_t, delivery_platform_in)
                on duplicate key update user_id = user_id_in, last_register = now_t;  
END */;;

/*!50003 SET SESSION SQL_MODE=@OLD_SQL_MODE */;;
# Dump of PROCEDURE search_users
# ------------------------------------------------------------

/*!50003 DROP PROCEDURE IF EXISTS `search_users` */;;
/*!50003 SET SESSION SQL_MODE=""*/;;
/*!50003 CREATE*/ /*!50020 DEFINER=`devuser`@`%`*/ /*!50003 PROCEDURE `search_users`(
	in search_term_in varchar(50)
)
    READS SQL DATA
begin
	select user_id, display_name, miniclip_id, level, total_winnings from user where (miniclip_id LIKE search_term_in) or (display_name LIKE search_term_in) limit 25;
end */;;

/*!50003 SET SESSION SQL_MODE=@OLD_SQL_MODE */;;
# Dump of PROCEDURE search_user_by_uid
# ------------------------------------------------------------

/*!50003 DROP PROCEDURE IF EXISTS `search_user_by_uid` */;;
/*!50003 SET SESSION SQL_MODE=""*/;;
/*!50003 CREATE*/ /*!50020 DEFINER=`devuser`@`%`*/ /*!50003 PROCEDURE `search_user_by_uid`(
	in uid_in varchar(64),
	in type_in tinyint
)
    READS SQL DATA
begin
	select user_id from uid where uid = uid_in and type = type_in limit 1;
end */;;

/*!50003 SET SESSION SQL_MODE=@OLD_SQL_MODE */;;
# Dump of PROCEDURE set_display_name
# ------------------------------------------------------------

/*!50003 DROP PROCEDURE IF EXISTS `set_display_name` */;;
/*!50003 SET SESSION SQL_MODE=""*/;;
/*!50003 CREATE*/ /*!50020 DEFINER=`devuser`@`%`*/ /*!50003 PROCEDURE `set_display_name`(
	in user_id_in int,
	in new_display_name_in varchar(50)
)
    MODIFIES SQL DATA
begin
	update user set display_name = new_display_name_in where user_id = user_id_in;
end */;;

/*!50003 SET SESSION SQL_MODE=@OLD_SQL_MODE */;;
# Dump of PROCEDURE start_match
# ------------------------------------------------------------

/*!50003 DROP PROCEDURE IF EXISTS `start_match` */;;
/*!50003 SET SESSION SQL_MODE=""*/;;
/*!50003 CREATE*/ /*!50020 DEFINER=`devuser`@`%`*/ /*!50003 PROCEDURE `start_match`(
	in user_a_id_in int unsigned,
	in user_b_id_in int unsigned,
	in tier_in int
)
    MODIFIES SQL DATA
begin
    declare user_not_found condition for sqlstate '99001';
	declare foreign_key_violated condition for 1452;
        
    declare continue handler for foreign_key_violated
    begin
    	signal user_not_found set message_text = 'User not found';
   	end;
	insert into match_history (user_a, user_b, tier, start) values (user_a_id_in, user_b_id_in, tier_in, now());
	select last_insert_id() as game_id;
end */;;

/*!50003 SET SESSION SQL_MODE=@OLD_SQL_MODE */;;
# Dump of PROCEDURE unregister_token
# ------------------------------------------------------------

/*!50003 DROP PROCEDURE IF EXISTS `unregister_token` */;;
/*!50003 SET SESSION SQL_MODE=""*/;;
/*!50003 CREATE*/ /*!50020 DEFINER=`devuser`@`%`*/ /*!50003 PROCEDURE `unregister_token`(
	in token_in VARCHAR(200)
)
    MODIFIES SQL DATA
BEGIN
	delete from push_token where token = token_in;
END */;;

/*!50003 SET SESSION SQL_MODE=@OLD_SQL_MODE */;;
DELIMITER ;

/*!40111 SET SQL_NOTES=@OLD_SQL_NOTES */;
/*!40101 SET SQL_MODE=@OLD_SQL_MODE */;
/*!40014 SET FOREIGN_KEY_CHECKS=@OLD_FOREIGN_KEY_CHECKS */;
/*!40101 SET CHARACTER_SET_CLIENT=@OLD_CHARACTER_SET_CLIENT */;
/*!40101 SET CHARACTER_SET_RESULTS=@OLD_CHARACTER_SET_RESULTS */;
/*!40101 SET COLLATION_CONNECTION=@OLD_COLLATION_CONNECTION */;
