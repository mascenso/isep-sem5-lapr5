describe('Teste da Tabela da lista de edificios', () => {
  
    beforeEach(() => {
        //criar edificio para ver na tabela
        cy.visit('http://localhost:4200/login');
        cy.get('#role').select('Administrador');
        cy.get('#signUp').click();
        cy.get('[id^="Campus"]').click();
        cy.get('[routerlink="create-building"]').click();
        const numeroAleatorio = Math.floor(1000 + Math.random() * 9000);
        cy.get('#mat-input-0').type('A'+numeroAleatorio.toString());
        cy.get('#mat-input-1').type('10');
        cy.get('#mat-input-2').type('50');
        cy.get('#mat-input-3').type('Edificio A');
        cy.get('#mat-input-4').type('Edificio A - Administracao');
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
        cy.get('.mdc-data-table__content > :last-child > .cdk-column-name').invoke('text').should('eq', ' Edificio A ');
        cy.get('.mdc-data-table__content > :last-child > .cdk-column-description').invoke('text').should('eq', ' Edificio A - Administracao ');
        cy.get('.mdc-data-table__content > :last-child > .cdk-column-maxLength').invoke('text').should('eq', ' 50 ');
        cy.get('.mdc-data-table__content > :last-child > .cdk-column-maxWidth').invoke('text').should('eq', ' 10 ');
    });
  
  });