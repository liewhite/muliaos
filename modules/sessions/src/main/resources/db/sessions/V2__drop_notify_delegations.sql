-- 回答推给用户的委派不用交回，不再记委派（委派方式只有 report / ask，见 TrackedMode）；迁移进来的旧记录都已结束，删掉。
DELETE FROM delegation WHERE mode = 'notify';
