describe('Teste da Tabela da lista de edificios', () => {
  
    beforeEach(() => {
        //criar edificio para ver na tabela
        cy.visit('http://localhost:4200/login');
        cy.get('#login-input-username').type('admin@email.pt');
        cy.get('#login-input-password').type('admin');
        cy.get('button[type="submit"]').click();
        cy.get('[id^="Campus"]').click();
        cy.get('[routerlink="create-building"]').click();
        const numeroAleatorio = Math.floor(1000 + Math.random() * 9000);
        cy.get('#mat-input-9').type('A'+numeroAleatorio.toString());
        cy.get('#mat-input-10').type("10");
        cy.get('#mat-input-11').type('10');
        cy.get('#mat-input-12').type('teste');
        cy.get('#mat-input-13').type('Mais um edificio de teste');
        cy.get('.mdc-button__label').click();
        //ir para a pagina correta do teste
        cy.get('[id^="Campus"]').click();
        cy.get('[routerlink="list-buildings"]').click();
    });
  
    
    it('Deve exibir a tabela corretamente e os valores da primeira coluna que vai buscar a bd', () => {

        //compara com o ultimo da tabela pois é esse que criamos
        cy.get('table.mat-elevation-z8').should('exist');
        cy.get('.mat-mdc-header-row > .cdk-column-code').should('exist');
        cy.get('.mat-mdc-header-row > .cdk-column-name').should('exist');
        cy.get('.mat-mdc-header-row > .cdk-column-description').should('exist');
        cy.get('.mat-mdc-header-row > .cdk-column-maxLength').should('exist');
        cy.get('.mat-mdc-header-row > .cdk-column-maxWidth').should('exist');
    });
  
  });