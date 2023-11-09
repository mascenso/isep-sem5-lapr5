const { spawn } = require('child_process');

// Função para executar `npm install` seguido de `npm run start`
function runNpmInstallAndStart(directory, appName, color) {
  const installProcess = spawn('npm', ['install'], { cwd: directory });

  installProcess.on('close', (installCode) => {
    if (installCode === 0) {
      const startProcess = spawn('npm', ['run', 'start'], { cwd: directory });

      startProcess.stdout.on('data', (data) => {
        console.log(`\x1b[${color}m[${appName}]\x1b[0m ${data.toString()}`);
      });

      startProcess.stderr.on('data', (data) => {
        console.error(`\x1b[${color}m[${appName}]\x1b[0m ${data.toString()}`);
      });

      startProcess.on('close', () => {
        console.log(`\x1b[${color}mIniciando o ${appName}...\x1b[0m`);
      });
    } else {
      console.error(`Erro ao executar 'npm install' em ${appName}`);
    }
  });
}

console.log('Iniciando o projeto...');

// Iniciar a UI com cor verde
runNpmInstallAndStart('./Frontend', 'UI Module', '32');

// Iniciar o backend com cor azul
runNpmInstallAndStart('./Gestao_Informacao', 'Information Module', '34');

