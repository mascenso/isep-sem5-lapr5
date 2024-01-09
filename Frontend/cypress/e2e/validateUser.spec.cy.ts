describe('Teste de validar Utilizador', () => {
  
    /*
    beforeEach(() => {
        cy.visit('http://localhost:4200/login');
        cy.get('#login-input-username').type('admin@email.pt');
        cy.get('#login-input-password').type('admin');
        cy.get('button[type="submit"]').click();
        cy.get('[id^="User"]').click();
        cy.get('[routerlink="create-user"]').click();
        cy.get('.create-user-container').within(() => {
            cy.get('.form-field-email input').type('newuser'+Math.floor(1000 + Math.random() * 9000).toString()+'@example.com');
            cy.get('.form-field-password input').type('NewPassword123!');
            cy.get('.form-field-firstName input').type('João');
            cy.get('.form-field-lastName input').type('Silva');
            cy.get('.form-field-user-role mat-select').click(); // Abrir o dropdown
            cy.get('.form-field-user-role mat-select').type('{downarrow}'); // Navegue para cima para ir até a última opção
            cy.get('.form-field-user-role mat-select').type('{downarrow}'); // Volte para baixo para a última opção
            cy.get('.form-field-user-role mat-select').type('{downarrow}'); // Volte para baixo para a última opção
            cy.get('.form-field-user-role mat-select').type('{enter}');
        });
        cy.get('.form-button-group button[type="submit"]').click(); // Enviar o formulário
        cy.get('[id^="User"]').click();
        cy.get('[routerlink="validate-user"]').click();

    });
    */

    it('Deve verificar a existência da tabela e dos elementos', () => {
        // Register a new User
        cy.visit('http://localhost:4200/register');
        const userData = {
            email: 'newuser@example.com',
            password: 'Password123!',
            firstName: 'John',
            lastName: 'Doe',
            taxPayerNumber: '123456789',
            mechanographicNumber: 'ABC1234',
            phoneNumber: '987654321',
        };
        cy.get('.form-field-email input').type('newuser'+Math.floor(1000 + Math.random() * 9000).toString()+'@example.com');
        cy.get('.form-field-password input').type('Password123');
        cy.get('.form-field input[formControlName="firstName"]').type(userData.firstName);
        cy.get('.form-field input[formControlName="lastName"]').type(userData.lastName);
        cy.get('.form-field input[formControlName="taxPayerNumber"]').type('123456789');
        cy.get('.form-field input[formControlName="mechanographicNumber"]').type(userData.mechanographicNumber);
        cy.get('.form-field input[formControlName="phoneNumber"]').type(userData.phoneNumber);
        cy.get('.terms-checkbox mat-checkbox').click(); // Assuming this checkbox signifies acceptance of terms
        cy.get('.btn-register-register').click();

        // Login with admin
        cy.visit('http://localhost:4200/login');
        cy.get('#login-input-username').type('admin@email.pt');
        cy.get('#login-input-password').type('admin');
        cy.get('button[type="submit"]').click();
        cy.get('[id^="User"]').click();
        cy.get('[routerlink="validate-user"]').click();
        cy.get('.validate-user-container').within(() => {
          cy.get('table').should('exist'); // Verifica se a tabela existe
    
          // Verifica se os elementos específicos existem na tabela
          cy.get('th[mat-header-cell]').should('exist'); // Verifica se há células de cabeçalho
          cy.get('td[mat-cell]').should('exist'); // Verifica se há células de dados
          cy.get('button[mat-icon-button]').should('exist'); // Verifica se há botões de ação
        });
    });

    
    it('Deve simular a aprovação de um usuário', () => {
        cy.visit('http://localhost:4200/login');
        cy.get('#login-input-username').type('admin@email.pt');
        cy.get('#login-input-password').type('admin');
        cy.get('button[type="submit"]').click();
        cy.get('[id^="User"]').click();
        cy.get('[routerlink="validate-user"]').click();
        cy.get('.validate-user-row').first().as('firstUserRow');
      
        cy.get('@firstUserRow').find('button[aria-label="expand row"]').click();
      
        cy.get('.user-approval button[color="primary"]')
            .should('contain.text', 'Accept')
            .should('be.visible')
            .click();
      
    });
    

    it('Deve simular a rejeição de um utilizador', () => {
        cy.visit('http://localhost:4200/register');
        const userData = {
            email: 'newuser@example.com',
            password: 'Password123!',
            firstName: 'John',
            lastName: 'Doe',
            taxPayerNumber: '123456789',
            mechanographicNumber: 'ABC1234',
            phoneNumber: '987654321',
        };
        cy.get('.form-field-email input').type('newuser'+Math.floor(1000 + Math.random() * 9000).toString()+'@example.com');
        cy.get('.form-field-password input').type('Password123');
        cy.get('.form-field input[formControlName="firstName"]').type(userData.firstName);
        cy.get('.form-field input[formControlName="lastName"]').type(userData.lastName);
        cy.get('.form-field input[formControlName="taxPayerNumber"]').type('123456789');
        cy.get('.form-field input[formControlName="mechanographicNumber"]').type(userData.mechanographicNumber);
        cy.get('.form-field input[formControlName="phoneNumber"]').type(userData.phoneNumber);
        cy.get('.terms-checkbox mat-checkbox').click(); // Assuming this checkbox signifies acceptance of terms
        cy.get('.btn-register-register').click();
        
        cy.visit('http://localhost:4200/login');
        cy.get('#login-input-username').type('admin@email.pt');
        cy.get('#login-input-password').type('admin');
        cy.get('button[type="submit"]').click();
        cy.get('[id^="User"]').click();
        cy.get('[routerlink="validate-user"]').click();
        cy.get('.validate-user-row').first().as('firstUserRow');
      
        cy.get('@firstUserRow').find('button[aria-label="expand row"]').click();
      
        cy.get('.user-approval button[color="warn"]')
            .should('contain.text', 'Reject')
            .should('be.visible')
            .click();
      
    });
      
})