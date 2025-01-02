| cuenta |

"Crear una cuenta con un saldo inicial de 1000"
cuenta := CuentaBancaria new init: 1000.

"Depositar dinero en la cuenta"
cuenta depositar: 500.  "Saldo: 1500"

"Retirar dinero de la cuenta"
cuenta retirar: 200.  "Saldo: 1300"

"Ver el saldo actual"
Transcript show: 'Saldo final: ', cuenta verSaldo printString; nl.

cuenta saldo.  "Error: saldo es privado y no se puede acceder directamente"