### R program

## http://sidc.oma.be/sunspot-data/dailyssn.php�ɂ��鑾�z���_���̃f�[�^��ǂݍ���(��q��Perl�v���O�����ɂ����2�񂾂��𔲂��o�����f�[�^)
df <- read.table("dayssn_new.dat", header=F)
colnames(df) <- c("year", "sunspots")

## ���ȑ��֊֐��̌v�Z
a <- acf(df, type="correlation", plot=TRUE)

## �X�y�N�g�����x�̌v�Z
s <- spec.pgram(df)

## ���ꂼ��̃O���t���ȉ��Ɏ����B
