DELIMITER $$

DROP PROCEDURE IF EXISTS `spkdb`.`folder_tree`$$
CREATE PROCEDURE `spkdb`.`folder_tree` (userId INT,parentId INT,INOUT tree TEXT)
BEGIN
    DECLARE folderId INT;
    DECLARE folderName TEXT;
    DECLARE done INT DEFAULT 0;

    DECLARE cur CURSOR FOR
      SELECT folder_id,name FROM folder WHERE user_id=userId AND parent=parentId;

    DECLARE CONTINUE HANDLER FOR NOT FOUND SET done=1;
    
    OPEN cur;
    folder_loop:LOOP
        
        FETCH cur INTO folderId,folderName;
        IF done=1 THEN
            LEAVE folder_loop;
        END IF;
        SET tree=CONCAT(tree,'<n',folderId,' name="',folderName,'">');
        CALL folder_tree(userId,folderId,tree);
        SET tree=CONCAT(tree,'</n',folderId,'>');

    END LOOP folder_loop;
    CLOSE cur;
END$$

DELIMITER ;
