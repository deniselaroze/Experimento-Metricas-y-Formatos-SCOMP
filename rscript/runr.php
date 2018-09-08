<?php
header('Content-Type: text/html; charset=utf-8');

$gender = getGender();
$econ = getEcon();
$mode1Q = getMode1Q();
$mode2Q = getMode2Q();
$pg = getPg();

// execute R script from shell

$command = "Rscript /var/www/r.cess.cl/public_html/Treatment-Script.R $gender $econ $mode1Q $mode2Q $pg";
$out = trim(shell_exec($command));

#echo(explode(',', $out));
$pagos = explode(',', $out);

echo "pay10=" . (string)$pagos[0] . "&";
echo "pay11=" . $pagos[1] . "&";
echo "pay12=" . $pagos[2] . "&";
echo "pay13=" . $pagos[3] . "&";
echo "pay14=" . $pagos[4] . "&";
echo "pay15=" . $pagos[5] . "&";
echo "pay16=" . $pagos[6] . "&";
echo "pay17=" . $pagos[7] . "&";
echo "pay18=" . $pagos[8] . "&";
echo "pay19=" . $pagos[9] . "&";
echo "pay110=" . $pagos[10] . "&";
echo "pay111=" . $pagos[11] . "&";
echo "pay112=" . $pagos[12] . "&";
echo "pay113=" . $pagos[13] . "&";
echo "pay114=" . $pagos[14] . "&";
echo "pay115=" . $pagos[15] . "&";
echo "pay21=" . $pagos[16] . "&";
echo "pay22=" . $pagos[17] . "&";
echo "pay23=" . $pagos[18] . "&";
echo "pay24=" . $pagos[19] . "&";
echo "pay25=" . $pagos[20] . "&";
echo "pay26=" . $pagos[21] . "&";
echo "pay27=" . $pagos[22] . "&";
echo "pay28=" . $pagos[23] . "&";
echo "pay29=" . $pagos[24] . "&";
echo "pay210=" . $pagos[25] . "&";
echo "pay211=" . $pagos[26] . "&";
echo "pay212=" . $pagos[27] . "&";
echo "pay213=" . $pagos[28] . "&";
echo "pay214=" . $pagos[29] . "&";
echo "treatv1=" . $pagos[30] . "&";
echo "treatv2=" . $pagos[31];

#======================================================================================

function getGender(){
	if(isset($_GET['gender'])){
		$str = trim($_GET['gender']);
		if(is_string($str)){
			return $str;
		}else{
			return null;
		}
	}else{
		return null;
	}
}

function getEcon(){
	if(isset($_GET['econ'])){
		$str = trim($_GET['econ']);
		if(is_string($str)){
			return $str;
		}else{
			return null;
		}
	}else{
		return null;
	}
}

function getMode1Q(){
	if(isset($_GET['mode1Q'])){
		$str = trim($_GET['mode1Q']);
		if(is_numeric($str)){
			return $str;
		}else{
			return null;
		}
	}else{
		return null;
	}
}

function getMode2Q(){
	if(isset($_GET['mode2Q'])){
		$str = trim($_GET['mode2Q']);
		if(is_numeric($str)){
			return $str;
		}else{
			return null;
		}
	}else{
		return null;
	}
}

function getPg(){
	if(isset($_GET['pg'])){
		$str = trim($_GET['pg']);
		if(is_string($str)){
			return $str;
		}else{
			return null;
		}
	}else{
		return null;
	}
}

function writeLog($msg){

	@file_put_contents('./runr.log', date('Y-m-d h:i:s') . "\t" . $gender . "\t" . $msg . "\n");
}

?>

